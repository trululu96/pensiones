# script con las funciones para la parametrización.



# Parametrizacion regulatoria ---------------------------------------------

#' Rentabilidad minima 
#' Esta funcion crea un vector con la parametrizacion regulatoria asociada al
#' a la rentabilidad minima.
#' 
#' @param .variableMedicion Define cual es la variable de medición  esta puede
#' ser: "nominal" o "real"
#' @param .b puntos básicos por debajo del benchmark sobre los que empieza a
#' aplicar la RM
#' @param .frecuenciaMedicion frecuencia en meses con la que se mide la 
#' rentabilidad mínima
#' @param .pMedicion numero de meses hasta la primera medicion 
#'
#' @return lista con las caracteristicas de rentabilidad minima
#' 
#' @export
#'
#' @examples
#' RM()
RM <- function(
  .variableMedicion = "nominal", 
  .b                = 0.01,
  .frecuenciaMedicion = 3,
  .pMedicion        = 12
  ){
  
  aux <-  list(
    .variableMedicion,
    .b,
    .frecuenciaMedicion,
    .pMedicion
  )
  
  names(aux) <- c(
    "variableMedicion",
    "b",
    "frecuenciaMedicion",
    "pMedicion"
  ) 
  
  
  return(aux)  
  
}

#' Reserva de estabilización
#' 
#' Esta función define las características de la reserva de estabilización.
#'
#' @param .parametro corresponde al nivel de confianza con el que trabajan los
#' afiliados 
#' 
#' @return lista con las características de la reserva de estabilización. 
#' @export
#'
#' @examples
#' RE()
RE <- function(
  .parametro = 0.99
  ){
  
  aux <-  list(
      .parametro
      )
  
  names(aux) <- c(
    "parametro"
  )
  
  return(aux)
  
}

#' Comisión por desempeño 
#' 
#' Define los parámetros de la comisión por desempeño
#'
#' @param .umbral sobre que umbral de la RM se empieza a pagar comisión por
#' desempeño (CD)
#' @param .pago que porcentaje se paga sobre las ganancias por encima del umbral
#' @param .portafolioAlternativo portafolio en el cual está invertido la porción
#' de comision por desempeño que aun no se paga. 
#' @param .pPago porcentaje de la comisión por desempeño que se paga
#' directamente a la administradora. 
#'
#' @return lista con las características de la comision por desempeño
#' @export
#'
#' @examples
#' CD()
CD <- function(
  .umbral = 0.0,
  .pago   = 0.0,
  .portafolioAlternativo = NA,
  .pPago = 0.8
){
  
  aux <- list(
      .umbral,
      .pago,
      .portafolioAlternativo,
      .pPago
    )
  
  names(aux) <- c(
    "umbral",
    "pago",
    "portafolioAlternativo",
    "pPago"
  )
  
  return(aux)
  
}


#' Parametros Regulación 
#' 
#' funcion para agregar los parametros. Cada uno de los componentes tiene 
#' una funcion diferente. 
#' 
#' ver cada una de la funciones con el nombre de los parametros, estas contienen
#' información específica.
#'
#' @param .RE
#' @param .RM
#' @param .CD
#'
#' @return lista con los parametros
#' @export
#'
#' @examples
#' 
parametrosRegulacion <- function(
  .RE         = RE(), 
  .RM         = RM(), 
  .CD         = CD() 
) { 
  
  aux <- list(
      .RE,
      .RM,
      .CD
    )
  
  
  names(aux)  <- 
    c(
      "RE",
      "RM",
      "CD"
    )
  
  return(aux) 
  }



# AFP -------------------------------------------------------------------


#' Función wrapper para introducir los afiliados
#' 
#' En esta función se pueden agregar nuevos parámetros que definan a una 
#' AFP. 
#' 
#' @param .funcionUtilidad función de utilidad que representan las preferencias
#' de la AFP.
#' 
#' @return lista de caracteristicas de las AFP.
#' @export
#'
#' @examples
#' 
#' 
parametrosAFP <- function(
  .funcionUtilidad = {\(x) x}
){
  aux <- list(
    .funcionUtilidad
  )
  
  names(aux) <- c(
    "funcionUtilidad"
  )
  
  return(aux)
}



