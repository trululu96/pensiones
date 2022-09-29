# en este script se cargan los datos:
# se cargan los datos y se eliminan los nombres de las simulaciones
# requiere Lubridate, abind y readxl

library(lubridate)
library(abind)
library(readxl)

#' Funcion para cargar los datos 
#'
#' @param .auxPath path de la carpeta donde se encuentran los archivos con la 
#' econometria
#' @param .nombreArchivo nombre (con el path absoluto) del cubo de simulaciones 
#'
#' @return no se retorna nada
#' @export
#'
#' @examples
procesarDatos <- function(
  .auxPath,
  .nombreArchivo
){


  assetClasses  <- readRDS(paste0(.auxPath, "AssetsTrmAhorros.rds"))

  dimnames(assetClasses)[[3]]  <- NULL

  # se cambian los activos a pesos. 


  assetClasses["MXWD INDEX",,]  <- assetClasses["MXWD INDEX",,] * 
    assetClasses["USDCOP Curncy",,]

  assetClasses["LBUSTRUU INDEX",,]  <- assetClasses["LBUSTRUU INDEX",,] *
    assetClasses["USDCOP Curncy",,]

  assetClasses["IPRV LN Equity",,]  <- assetClasses["IPRV LN Equity",,] * 
    assetClasses["USDCOP Curncy",,]
    

  # bonosSinteticos

  BonosSinteticos  <- readRDS(paste0(.auxPath, "BonosSinteticosCon70.rds")) 

  # se realiza una correción del orden de las dimensiones.


  dimnames(BonosSinteticos)[[3]]  <- NULL


  # variables Macro

  variablesMacro  <- readRDS(paste0(.auxPath, "VariablesMacro.rds")) 

  dimnames(variablesMacro)[[3]]  <- NULL

  # se ajustan los meses para que queden igual en todas las simulaciones

  # se tienen que ajustar los nombres de las fechas de las simulaciones

  ajustarNombres <- function(
      .nombres
  ){
    .nombres <- .nombres |>
    as.Date() |>
    ceiling_date(unit = "month")

    .nombres <- .nombres - 1 

    .nombres <- .nombres |>
      as.character()
    
    return(.nombres)

  }

  dimnames(assetClasses)[[2]] <- ajustarNombres(
    dimnames(assetClasses)[[2]]
    )

  dimnames(BonosSinteticos)[[2]] <- ajustarNombres(
    dimnames(BonosSinteticos)[[2]]
    )

  dimnames(variablesMacro)[[2]] <- ajustarNombres(
    dimnames(variablesMacro)[[2]]
    )


  fechaInicial  <- max(
      dimnames(assetClasses)[[2]] |> as.Date() |> min(), 
      dimnames(BonosSinteticos)[[2]] |> as.Date() |> min(), 
      dimnames(variablesMacro)[[2]] |> as.Date() |> min()
  )  |> as.character()

  assetClasses <- assetClasses[,
  which(dimnames(assetClasses)[[2]] == fechaInicial)[1]:dim(assetClasses)[2],
      ]

  BonosSinteticos <- BonosSinteticos[,
  which(dimnames(BonosSinteticos)[[2]] == fechaInicial)[1]:dim(BonosSinteticos)[2],
      ]

  variablesMacro <- variablesMacro[,
  which(dimnames(variablesMacro)[[2]] == fechaInicial)[1]:dim(variablesMacro)[2],
      ]



  # se crear un array de monedas

  monedas <- array(1, dim = c(2, dim(assetClasses)[2:3])) 

  dimnames(monedas)[[1]]  <- c("nominal", "real")

  dimnames(monedas)[[2]]  <- dimnames(assetClasses)[[2]]

  # ahora se agregan los valores del IPC
  
  # !!!! CAMBIARLO

  IPCnivel  <- read_xlsx(
    "/Users/daniel.gomez/Google Drive/Unidades compartidas/URF - Regulación Fondos de Pensiones/Fase 2/Datos/Variables Macro/IPCnivel.xlsx"
  ) |>
  data.frame()

  IPCnivel[["Fecha"]] <- ajustarNombres(IPCnivel[["Fecha"]])

  # se determina la fecha inicial de este estudio.

  ini <- which(IPCnivel[["Fecha"]] == fechaInicial)[1]

  monedas["real", 1:12, ]  <- IPCnivel[["IPC"]][ini:(ini + 11)]


  for(i in 13:24){
    counter  <- i
    while(counter <= dim(variablesMacro)[2]){
      monedas["real", counter, ]  <- 
      monedas["real", counter - 12, ] *
      (variablesMacro["Inflacion", counter , ] + 1)
      counter <- counter + 12
    }
  }


  #### Array final ####

  assetClasses <- assetClasses[which(dimnames(assetClasses)[[1]] != "USDCOP Curncy"),,]
  
  # se crea el array con lo precios de asset classes

  resultado <- abind(assetClasses, BonosSinteticos, along = 1 ) 
  
  # se crea el resultado 

  resultado <- abind(resultado, monedas, along = 1)


  # los asset classes que estén en UVR se deben pasar a COP (todo tiene que estar
  # en COP)

  UVR <- which(substr(rownames(resultado), 1, 3) == "UVR")

  # se crea un índice alternativo.

  # tener en cuenta que los UVR se mueven con el IPC del mes anterior 
  # por lo tanto se tiene que 

  IPC <- resultado["real",,]/resultado["real",1,]

  for(i in UVR){
    resultado[i,,] <- resultado[i,,] * IPC
  }

  # no se retorna nada, sino que se guarda el archivo
  
  resultado |> 
    saveRDS(.nombreArchivo)
}



# corrida -----------------------------------------------------------------

# se realiza el procesamiento de los datos


pathBase <- "/Users/daniel.gomez/Google Drive/Unidades compartidas/URF - Regulación Fondos de Pensiones/Fase 2/Programación/URF_ECONOMETRIA/output/"
auxPath <- paste0(pathBase, "sims_non_afiliates_am/")
procesarDatos(
  auxPath, 
  "/Users/daniel.gomez/Google Drive/Mi unidad/Organizado/URF/URF_AFP/datos/simulaciones.rds"
  ) 
