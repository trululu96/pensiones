# En este archivo se extraen todos los datos provenientes de la SFC.

# tidy up the environment and go to the correct directory

rm(list=ls())

setwd("/Volumes/GoogleDrive/Mi unidad/Organizado/URF/URF_AFP/")

# load packages

c(
  "dplyr",
  "readxl", 
  "tibble",
  "stringi",
  "ggplot2",
  "tidyr",
  "lubridate"
) |> 
  sapply(require, character.only = TRUE)



# PUC ---------------------------------------------------------------------
source("./codigo/extraccion/unirPUC.r", encoding = "UTF-8")


cuentas <- c(
  510000, # 1
  517000, # 2
  512500, # 3
  512900, # 4
  513900, # 5
  510500, # 6
  510600, # 7
  510700, # 8
  510800, # 9
  511527, # 10
  511524, # 11
  512000, # 12
  411528  # 13
)

titulos <- c(
  "Gastos operacionales", # 1
  "Deterioro", # 2
  "Venta Inversiones",  # 3
  "Derivado Negociacion", # 4
  "Derivados Coberturas", # 5
  "VentaBienes",  # 6 
  "Inversiones1", # 7
  "Inversiones Amortizado", # 8
  "Inversiones2", # 9
  "Gastos Pensiones", # 10
  "Gastos Cesantias", # 11
  "Empleados", # 12
  "IngresosComisiones" # 13
)

# lee la funcion 

if("PUC.rds" %in% list.files("./data/binarios/")){
  PUC <- readRDS("./data/binarios/PUC.rds")
}else{
  
  PUC <- WrapPUC(cuentas, titulos)
  
  saveRDS(PUC,"./data/binarios/PUC.rds")
}

# GastosTotales 

totalesNegativos <- c(
  517000, # 2
  512500, # 3
  512900, # 4
  513900, # 5
  510500, # 6
  510600, # 7
  510700, # 8
  510800, # 9
  511524  # 11
) |> 
  as.character()

totalesEspecificos <- c(
  511527 # 10
) |> 
  as.character()

totalesFijos <- c(
  512000  # 12
) |> 
  as.character()

ingresos <- c(
  411528  # 13
) |> 
  as.character()

# Se realiza una división entre gastos fijos, especificos (al negocio
# de pensiones obligatorias) y gastosgenerales (variables) 

# se requiere dejar los flujos en pesos constantes. Por facilidad se dejan en 
# pesos de 2021. Solo se descuenta una vez por año (esto supone que solo hay 
# un ajuste de precios una vez al año)


GastosFijos <- lapply(
  totalesFijos |> as.list(), 
  {\(x) extraerPUC(x, PUC)}
  ) |> 
  {\(x) Reduce(`+`, x)}() 

GastosEspecificos <- lapply(
  totalesEspecificos |> as.list(), 
  {\(x) extraerPUC(x, PUC)}
  ) |> 
  {\(x) Reduce(`+`, x)}() 
 
GastosGenerales <- lapply(
  totalesNegativos |> as.list(), 
  {\(x) extraerPUC(x, PUC)}
  ) |> 
  {\(x) Reduce(`+`, x)}() |> 
  {\(x) extraerPUC("510000", PUC) - x - GastosEspecificos - GastosFijos}()



orden <- GastosGenerales |> 
  colnames()



# Afiliados ---------------------------------------------------------------

# en esta sección se realiza el procesamiento de los afiliados a cada uno
# de los negocios para realizar la separación de los mismos.

# las fechas de todos tienen que ser iguales

# existe una gran estacionalidad en los afiliados, pero los costos no tienen esa
# flexibilidad, entonces se deja deja el promedio anual de los afiliados para
# cada uno de los productos. En adición, los costos que se consideran fijos se 
# crecen no solo con el número de afiliados, sino con el número de fondos que
# tengan que administrar las AFP.

promediar <- function(
  .afiliados
  
){
  .afiliados <- .afiliados |> 
    data.frame()
  
  .afiliados[["fechas"]] <- rownames(GastosGenerales) |> 
    as.Date()
  
  .afiliados <- .afiliados |> 
    group_by(
      fechas = year(`fechas`)
    ) |> 
    mutate(
      across(.cols = everything() , .fns = ~mean(.x))
    ) |> 
    ungroup() |> 
    select(
      !`fechas`
    ) |> 
    data.frame()
  
  
  return(.afiliados)
  
  
} 

# cesantias 

AfiliadosCesantias <- read_xlsx("./data/procesado/afiliadosCesantias.xlsx")

colnames(AfiliadosCesantias) <- colnames(AfiliadosCesantias) |> 
  tolower()

AfiliadosCesantias <- AfiliadosCesantias[,c("fecha", orden)] |> 
  data.frame() |> 
  mutate(
    fecha = paste0(`fecha`,"/01") |> 
      as.Date(format = "%Y/%m/%d") |>
      ceiling_date(unit = "month") |> 
      {\(x) x %m-% days(1)}() 
  )

# se extraen las fechas

maxR <- AfiliadosCesantias[["fecha"]] |> 
  as.character() |> 
  {\(x) which(x %in% rownames(GastosGenerales))}() |> 
  max()

  
  
AfiliadosCesantias <- AfiliadosCesantias[1:maxR, orden]  |> 
  promediar()

# voluntarias

AfiliadosVoluntarias <- read_xlsx("./data/procesado/afiliadosVoluntarias.xlsx")

colnames(AfiliadosVoluntarias) <- colnames(AfiliadosVoluntarias) |> 
  tolower()

AfiliadosVoluntarias <- AfiliadosVoluntarias[1:maxR, orden] |> 
  data.frame() |> 
  promediar()


# fondos de pensiones obligatorias


procesarObligatorias <- function(
  .x
){
  resultado <-  read_xlsx(
    "./data/procesado/afiliadosObligatorias.xlsx",
    sheet = .x
    ) |> 
    data.frame() |> 
    {\(z) z[,-1]}()

  colnames(resultado) <- colnames(resultado) |> 
    tolower()
  
  resultado <- resultado[1:maxR, orden]
  return(resultado)

}


# se realiza el supuesto de que las AFP tienen un costo constante por afiliado

Obligatorias <- Reduce(
  `+`,
  lapply(
    c("moderado", "riesgo", "conservador") |> as.list(), 
    procesarObligatorias
  )
) |> 
  promediar()


# se crea la proporcion de afiliados que se encuentran en pensiones
# obligatorias


TotalAfiliados <- AfiliadosVoluntarias +
  AfiliadosCesantias +
  Obligatorias 

# procesamiento de gastos -------------------------------------------------

# Estos gastos se supone que son iguales para cada uno de lso fondos 
# administrados en caso de aumentar el número de fondos, estos no dependerán
# del número de afiliados

GastosFijosPensiones <- (GastosFijos*Obligatorias/TotalAfiliados) |> 
  promediar()

GastosTotalPensiones <- (GastosGenerales*Obligatorias/TotalAfiliados + 
  GastosEspecificos + GastosFijosPensiones) |> 
  promediar()


coeficientes <- function(.x){
  datos <- matrix(NA, nrow = nrow(GastosFijosPensiones), ncol = 2) |> 
    data.frame()
  
  datos[,1] <- Obligatorias[[.x]]
  datos[,2] <- GastosTotalPensiones[[.x]]
  colnames(datos) <- c("afiliados", "gastos")
  
  resultado <- lm(`gastos`  ~ `afiliados`, datos, )[["coefficients"]]
  
  names(resultado) <- c("intercepto", "afiliados")
  
  aux <- GastosTotalPensiones - GastosFijosPensiones
  
  promedios <- GastosFijosPensiones |> 
    colMeans()
  
  datos[,2] <- aux[[.x]] - promedios[[.x]][1]
  
  resultados2 <- lm(`gastos`  ~ `afiliados` + 0, datos )[["coefficients"]]
  
  resultado <- c(
    resultado,
    promedios[[.x]][1],
    resultados2
  )
  
  names(resultado) <- c(
    "intercepto",
    "afiliados",
    "gastosFijos",
    "afiliadosConFijos"
    )
  
  return(resultado)
  
}


GastosAFP <- lapply(
  colnames(GastosFijosPensiones) |> as.list(), 
  coeficientes
  )


names(GastosAFP) <- colnames(GastosFijosPensiones)

# se guardan los datos procesados
saveRDS(
  GastosAFP,
  "./data/binarios/AFP.rds"
)



# se calculan los ingresos por afp de las comisiones por pensiones obligatorias

ingresos <- lapply(
  c("411528") |> as.list(), 
  {\(x) extraerPUC(x, PUC)}
) |> 
  {\(x) Reduce(`+`, x)}() 

# se copian los ingresos

ingresos |> 
  clipr::write_clip()

promediar(ingresos)[84,] |> round() |> clipr::write_clip()

Obligatorias[84,] |> round() |> clipr::write_clip()














