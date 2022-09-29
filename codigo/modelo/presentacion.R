# documento

rm(list = ls())

setwd("/Users/daniel.gomez/Google Drive/Mi unidad/Organizado/URF/URF_AFP/")

library(ggplot2)
library(dplyr)
library(stringr)


# funciones que permiten definir los parámetros
source("./codigo/modelo/funcionesParametros.r", encoding = "UTF-8")

# funcion de optimizacion general
source("./codigo/modelo/optimizacionGradienteFuncional.r", encoding = "UTF-8") 

# modelo de fiduciarias
source("./codigo/modelo/funcionesUtilidadEsperada.r", encoding = "UTF-8")

# wrappers para realizar optimizaciones complejas
# en este !!! script se crea una función denominada vPortafolioAux en esta están 
# escritos los nombres de los activos, en caso que esto vaya a cambiar se
# debería cambiar directamente en el script
# a su vez se crea una función "cPortafolioBenchmark" que sirve para crear 
# los portafolios benchmarks con un formato utilizable, esta se tiene que 
# cambiar en caso que cambien los asset classes.

source("./codigo/modelo/funcionesAux.r", encoding = "UTF-8")

# donde se encuentran los portafolios.

pathPortafolios <- paste0(
  "/Users/daniel.gomez/Google Drive/Unidades compartidas/",
  "URF - Regulación Fondos de Pensiones/Fase 2/Programación/URF_ECONOMETRIA/",
  "output/segmentation_non_afiliates_urf/" 
  ) 

pp <- function(x){
  paste0(pathPortafolios,x)
}


### funcion para guardar imagenes

# path donde tienen que quedar guardadas las imagenes
pathPictures <- paste0(
  "/Users/daniel.gomez/Google Drive/Mi unidad/Organizado/URF/URF_AFP/imagenes/" 
  ) 

sPicture <- function(x){
  ggsave(paste0(pathPictures, x,".png"))
}

# se realiza una iteración para cada segmento


iteracion <- function(
    .titulo,
    .aversion,
    .sufijo = "",
    .drift = 0 
){
  
  
  if(.sufijo == ""){
    
    pathPictures <- paste0(
      "/Users/daniel.gomez/Google Drive/Mi unidad/Organizado/URF/URF_AFP/",
      "imagenes/original/" 
    ) 
    
    datos  <- readRDS(
      "./datos/simulaciones.rds"
    )
    
  } else {
    
    pathPictures <- paste0(
      "/Users/daniel.gomez/Google Drive/Mi unidad/Organizado/URF/URF_AFP/",
      "imagenes/",
      .sufijo,
      "/"
    ) 
    
    datos  <- readRDS(
      paste0(
      "./datos/simulaciones",
      .sufijo,
      ".rds"
      )
    )
    
  }
  
  sPicture <- function(x){
    ggsave(paste0(pathPictures, x,".png"))
  }
  
  sCSV <- function(.x, .name){
    
    write.csv(
      .x,
      file = paste0(pathPictures, .name,".csv")
    )
  }
  
    
  # se realiza una adecuación de los datos, se dejan solo dos años 
  # para que se lleguen a duraciones más similares a los afiliados
  
  periodoActual <- 140
  
  datos <- datos[,1:(periodoActual + 24),]
    
    
  portafolio <- pp(
    paste0(
      "PortafolioIdoneo",
      .titulo,
      "Plazo",
      .sufijo,
      ".rds"
    )
  )
  
  up <- pp(
    paste0(
      "ValoresUPsYuvrEsperadoActivosFinalesCluster",
      .titulo,
      "Plazo.rds"
    )
  ) 
  
  ## optimizacion general
  
  
  FPT <- datosCompletos(
    .datos = datos,
    .aversionRiesgoEntes = .aversion,
    .aversionRiesgoEntidades = 13.25,
    .pathPortafolioOptimo = portafolio,
    .pathUP = up,
    .periodoActual = 140,
    .limiteTes = 0.5,
    .alphas = seq(0.01, 0.08, by = 0.01),
    .betas = seq(0 ,0.08, by = 0.01)
  ) |> 
    calculoTrackingError(
      .datos = datos,
      .aversionRiesgoEntidades = 13.25,
      .pathPortafolioOptimo = portafolio,
      .periodoActual = 140
    ) |> 
    calcularIngresos()
  
  # se guarda la grafica y los portafolios
  
  sCSV(
    FPT[["grafica"]],
    paste0(.titulo, "_grafica")
  )
  
  sCSV(
    FPT[["portafolio"]],
    paste0(.titulo, "_portafolio")
  )
    
  
  FPT[["grafica"]] |> 
    graficaEntidades(
      .ylab = "Equivalente AFP",
      .titulo = paste0("Segmento", .titulo, "plazo")
    )
  
  sPicture(paste0("eqAFP",.titulo))
  
  # en este punto se guarda un CSV con las graficas
  
  
  # se realizan múltiples iteraciones especificas
  
  for (i in seq(0.01, 0.08, by = 0.01)){
    altAlpha <- round(i*100) |> 
      as.character()
    
    
    FPT[["grafica"]] |> 
      graficarTEAlpha(
        .alpha = i,
        .ylab = "TE respecto a benchmark",
        .titulo = paste0(
          "Equivalencia TE para\nalpha = ", 
          altAlpha,
          "%, ",
          .titulo
        )
      )
    
    sPicture(
      paste0(
        "TE_",
        .titulo,
        "_",
        altAlpha
        )
      )
    
    FPT[["grafica"]] |> 
      graficarUnAlpha(
        .alpha = i,
        .ylab = "Equivalente afiliados",
        .titulo = paste0(
          "Equivalente afiliados para\nalpha = ", 
          altAlpha,
          "%, ",
          .titulo 
          ) 
        )
    
    
    sPicture(
      paste0(
        "afiliados_",
        altAlpha,
        "_",
        .titulo 
        )
      )
  }
  
  
  return(FPT)
  
}

# se realizan las iteraciones


vPortafolioAux <- function(...){
  lista <- list(...)

  lista <- lista |>
    sapply(I,simplify = "vector")

  nombres <- c(
    "MXWD INDEX",
    "LBUSTRUU INDEX",
    "COLCAP INDEX",
    "IPRV LN Equity",
    "Ahorros",
    "COP1",
    "COP3",
    "COP5",
    "COP10",
    "COP20",
    "COP30",
    "UVR1",
    "UVR3",
    "UVR5",
    "UVR10",
    "UVR20",
    "UVR30"
  )

  portafolioBase <- rep(0,length(nombres))

  names(portafolioBase) <- nombres

  portafolioBase[names(lista)]  <- lista

  # se normaliza el portafolio a uno

  portafolioBase <- portafolioBase / sum(portafolioBase)


  return(portafolioBase)

}

# originales 

# en este punto se realiza una paralelizacion, solo funciona en MAC.


wrapperIteracion <- function(
    .x
){
  
  iteracion(.x[[1]], .x[[2]])
  
}

# se crea una lista de lista con los parámetros:

listaOriginal <- list(
  list(
    "Corto",
    18.16
  ),
  list(
    "Mediano",
    16.15
  ),
  list(
    "Largo",
    13.11
  )
)

# en caso que el script se corra en un computador con sistema operativo windows 
# esto se debe cambiar por un lapply o alternativamente poner la opción 
# mc.cores = 1. No se deja automático el cambio para que falle y quien lo esté
# corriendo se de cuenta que esto es mucho más lento que como se escribió 
# originalmente. 

original <- lapply(
  listaOriginal,
  wrapperIteracion
)

saveRDS(
  original,
  paste0(pathPictures, "resultados.rds")
)



# con 70 años

vPortafolioAux <- function(...){
  lista <- list(...)

  lista <- lista |>
    sapply(I,simplify = "vector")
  
  nombres <- c(
    "MXWD INDEX",
    "LBUSTRUU INDEX",
    "COLCAP INDEX",
    "IPRV LN Equity",
    "Ahorros",
    "COP1",
    "COP3",
    "COP5",
    "COP10",
    "COP20",
    "COP30",
    "COP50",
    "COP70",
    "UVR1",
    "UVR3",
    "UVR5",
    "UVR10",
    "UVR20",
    "UVR30",
    "UVR50",
    "UVR70"
  )
  
  portafolioBase <- rep(0,length(nombres))

  names(portafolioBase) <- nombres

  portafolioBase[names(lista)]  <- lista

  # se normaliza el portafolio a uno

  portafolioBase <- portafolioBase / sum(portafolioBase)


  return(portafolioBase)

}


wrapperIteracion <- function(
    .x
){
  
  iteracion(.x[[1]], .x[[2]], .sufijo = .x[[3]])
  
}

# se crea una lista de lista con los parámetros:

listaAnos <- list(
  list(
    "Corto",
    18.16,
    "Con70anos"
  ),
  list(
    "Mediano",
    16.15,
    "Con70anos"
  ),
  list(
    "Largo",
    13.11,
    "Con70anos"
  )
)


anos  <- lapply(
  listaAnos,
  wrapperIteracion
)


saveRDS(
  anos,
  paste0(pathPictures, "resultados70anos.rds")
)




################################################################################
# cálculo de la reserva de estabilización para los portafolios actuales.

vPortafolioAux <- function(...){
  lista <- list(...)

  lista <- lista |>
    sapply(I,simplify = "vector")

  nombres <- c(
    "MXWD INDEX",
    "LBUSTRUU INDEX",
    "COLCAP INDEX",
    "IPRV LN Equity",
    "Ahorros",
    "COP1",
    "COP3",
    "COP5",
    "COP10",
    "COP20",
    "COP30",
    "UVR1",
    "UVR3",
    "UVR5",
    "UVR10",
    "UVR20",
    "UVR30"
  )

  portafolioBase <- rep(0,length(nombres))

  names(portafolioBase) <- nombres

  portafolioBase[names(lista)]  <- lista

  # se normaliza el portafolio a uno

  portafolioBase <- portafolioBase / sum(portafolioBase)


  return(portafolioBase)

}

# portafolios Actuales

PortActualLargo<-vPortafolioAux(COP1=5.29198E-05,COP3=0.000871544,COP5=0.002629216,COP10=0.014841055,COP20=0.05561446,
                                COP30=0.014330369,UVR1=0.00,UVR3=0.000,UVR5=0.0,UVR10=0.008032185,
                                UVR20=0.022216736,UVR30=0.005687068,"LBUSTRUU INDEX"=0.049164885,
                                "MXWD INDEX"=0.579558592,Ahorros=0.019501342,"COLCAP INDEX"=0.227499629)

PortActualMediano<-vPortafolioAux(COP1=0.000,COP3=0.00016,COP5=0.003188,COP10=0.040354,COP20=0.099883,
                                  COP30=0.017771,UVR1=0.000000,UVR3=0.0,UVR5=0.0,UVR10=0.078950,
                                  UVR20=0.079761,UVR30=0.007658,"LBUSTRUU INDEX"=0.072287,
                                  "MXWD INDEX"=0.417486,Ahorros=0.015846,"COLCAP INDEX"=0.166649)

PortActualCorto<-vPortafolioAux(COP1=0.005115655,COP3=0.01448476,COP5=0.020926597,COP10=0.18014342,COP20=0.162579071,
                                COP30=0.005806395,UVR1=0,UVR3=0,UVR5=0,UVR10=0.137428768,
                                UVR20=0.180194975,UVR30=0.001795273,"LBUSTRUU INDEX"=0.035551634,
                                "MXWD INDEX"=0.093020231,Ahorros=0.085847696,"COLCAP INDEX"=0.077105526)

  datos  <- readRDS(
    paste0(
    "./datos/simulaciones.rds"
    )
  )

  periodoActual <- 140
  
  datos <- datos[,1:(periodoActual + 24),]
  

IterarSobreBetas <- function(
    .datos,
    .portafolioActual,
    .betas,
    .lista,
    .benchmark
){ 
  
  reserva <- c()
  
  
  for(i in .betas){
    
    actual <- utilidadEsperada(
      .portafolioActual,
      .datos,
      140,
      .parametrosAFP = .lista[["optimizaciones"]][[1]][["regulacionAFP"]],
      .parametrosRegulacion =, parametrosRegulacion(
        .RE = RE(.parametro = 0.99),
        .RM = RM(.b = i),
        .CD = CD(.portafolioAlternativo = vPortafolioAux(Ahorros = 1))
          ),
      .portafolioBenchmark = .benchmark,
      .valorInicialPortafolio = 100,
      .portafolioEstabilizacion = .benchmark
    )[["reserva"]]
    
    reserva <- c(reserva, actual)
    
    
  }
  
  return(reserva)
  
  
  }




# orden Corto mediano largo

IterarSobreBetas(
  .datos = datos,
  .portafolioActual = PortActualLargo,
  .betas = seq(0,0.15, by = 0.01),
  .lista = original[[3]],
  .benchmark = cPortafolioBenchmark(
    readRDS(
      paste0(
        pathPortafolios,
        paste0(
          "PortafolioIdoneoLargoPlazo.rds"
        )
      )
    )
  )
)

IterarSobreBetas(
  .datos = datos,
  .portafolioActual = PortActualMediano,
  .betas = seq(0,0.15, by = 0.01),
  .lista = original[[2]],
  .benchmark = cPortafolioBenchmark(
    readRDS(
      paste0(
        pathPortafolios,
        paste0(
          "PortafolioIdoneoMedianoPlazo.rds"
        )
      )
    )
  )
)


IterarSobreBetas(
  .datos = datos,
  .portafolioActual = PortActualCorto,
  .betas = seq(0,0.15, by = 0.01),
  .lista = original[[1]],
  .benchmark = cPortafolioBenchmark(
    readRDS(
      paste0(
        pathPortafolios,
        paste0(
          "PortafolioIdoneoCortoPlazo.rds"
        )
      )
    )
  )
)

# portafolios

  



















