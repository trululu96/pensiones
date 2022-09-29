



#' gradiente
#' Función para realizar una optimización de una función de utilidad basado 
#' en gradiente descendiente. Esta optimización es restringida y consistente
#' con los límites establecidos por TES. Requiere como argumento una funcion que 
#' tome como primer argumento un portafolio y arroje el equivalente de certeza
#' para ese portafolio. (puede tener otros argumentos que serán pasados
#' a esta).
#' 
#'
#' @param .tolerancia Tolerancia para dejar de iterar
#' @param .maxIter maximo número de iteraciones
#' @param .portIni portafolio inicial desde el que se empieza a iterar. Dados 
#' los mínimos locales, el resultado de la función dependerá de este parámetro.
#' Tener en cuenta que este portafolio tiene que cumplir con la restricción 
#' de TES de lo contrario se producirá un error.
#' @param .funcionUE Funcion que toma como primer argumento un portafolio. Esta
#' tiene que ser autocontenida.
#' y retorna un equivalente de certeza.
#' @param .saltoInicial Cuanto se cambia el portafolio en la primera iteracion
#' @param .limiteTes Cuanto es el límite de tes
#' @param ... Otros argumentos nombrados que se tienen que pasar a 
#' la funcionUE
#'
#' @return Portafolio óptimo.
#' @export
#'
#' @examples
optimizarGradiente <- function(
  .tolerancia, 
  .maxIter, 
  .portIni,
  .funcionUE,
  .saltoInicial,
  .limiteTes = 0.5,
  ...
){
  
  
  wrapperUtilidad <- function(
    ..portafolio
  ){
    
    # este wrapper retorna el equivalente de certeza de una función de 
    # utilidad arbitraria. En este caso el equivalente de certeza se encuentra
    # en % (retornos, TIR ...)
    
    return(
      .funcionUE(
        ..portafolio, 
        ...
      )
    )
  }
  
  
  # se limitan los tes a un techo arbitrario
  
  limiteTes <- .limiteTes
  
  # que tan grande será el salto para calcular la gradiente
  
  funAuxCompleto <- function(..pos, ..portafolio, ..movimiento){
    
    resultado <- ..portafolio
    
    # se encuentra la proporcion que deben subir los demás activos para que
    # se llegue al límite.
    
    
    if ((..portafolio[..pos]  + ..movimiento) >= 1 ) {
      
      # se está muy cerca del límite
      # se retorna un portafolio con 1 en el activo en cuestión
      # los activos diferentes al activo en cuestión se deben dejar en 0.
      
      # se define el tamaño del salto.
      
      salto <- 1 - ..portafolio[..pos]
      
      resultado[-..pos] <- 0
      
      resultado[..pos] <- 1
      
    } else {
      
      # no se encuentra en uno de los límites
      
      bajada <- 1 - (..movimiento / sum(..portafolio[-..pos]))
      
      resultado[-..pos] <- resultado[-..pos] * bajada 
      
      # se actualiza el activo
      
      resultado[..pos] <- resultado[..pos] + ..movimiento
      
      # se define el tamaño del salto
      
      salto <- ..movimiento
      
    }
    
    # se retorna tanto el salto como el resultado, se tiene que tener en cuenta
    # que es
    
    return(resultado)
    
    
  } 
  
  cambiarPortafolioCompleto <- function(
    ..portafolio,
    ..movimiento
  ){
    
    
    
    resultadoCompleto <- lapply(
      seq_len(length(..portafolio)),
      funAuxCompleto,
      ..portafolio = ..portafolio,
      ..movimiento = ..movimiento
    )
    
    # se retorna el resultadoCompleto
    
    return(resultadoCompleto)
    
  }
  
  # en caso que se este pegado a una pared (límite de tes) se mueve el 
  # portafolio al rededor de los mismos.
  
  # !!! ojo tes tiene que ser un vector con las posiciones de los TES
  
  funAuxTES <- function(..pos, ..tes, ..portafolio, ..movimiento){
    
    
    # valor inicial del tes en la posicion ..pos
    
    auxValue <- ..portafolio[..pos]
    
    resultado <- ..portafolio
    
    # dentro de los TES cual es el que corresponde a la posicion
    
    tesActual <- which(..tes == ..pos)[1]
    
    # se calcula la suma de TES sin tener en cuenta la posicion que se va a 
    # aumentar
    
    otrosTes <- ..portafolio[..tes[-tesActual]]
    
    sumaTes <- sum(otrosTes) 
    
    
    
    if ( (auxValue + ..movimiento) >= limiteTes) {
      # se tienen que reducir todos los demás para que se pegue al límite
      # incluídos los no tes (porque de ahí es que van a salir los pesos
      # necesarios)
      
      
      # la bajada esta compuesta de dos 
      
      salto <- limiteTes - sumaTes - auxValue
      
      bajada <- 1 - (salto/sum(..portafolio[-tes]))
      
      
      resultado[-..pos] <- resultado[-..pos]*bajada
      
      resultado[..tes] <- 0
      
      resultado[..pos] <- limiteTes
      
    } else {
      # el tes en la posicion se encuentra a mas de un ..movimiento de el 
      # limite. 
      # Se cambian los demás tes proporcionalmente (en principio acá se
      # encuentra en el límite, entonces el movimiento saldrá de los demás
      # TES, los demás activos se conservan igual)
      
      bajada <- 1 - (..movimiento/sumaTes)
      
      
      resultado[..tes] <- resultado[..tes] * bajada
      
      resultado[..pos] <- auxValue + ..movimiento
      
    }
    
    return(resultado)
    
  } 
  
  
  cambiarTES <- function(
    ..portafolio,
    ..tes,
    ..movimiento
  ){
    
    
    resultadoCompleto <- lapply(
      as.list(..tes),
      funAuxTES,
      ..tes = ..tes,
      ..portafolio = ..portafolio,
      ..movimiento = ..movimiento
    )
    
    return(resultadoCompleto)
    
    
  }
  
  
  # se encuentran los activos que sean TES (UVR o COP), esto implica que no
  # pueden tener en el nombre ni COP ni UVR activos que no sean de estas 
  # categorías
  
  # tambien se requiere que el portafolio inicial tenga los nombres correctos.
  
  tes <- grep(
    "UVR|COP",
    names(.portIni)
  )
  
  # contador para definir el máximo de iteraciones y diferencia inicial
  
  contador <- 0
  
  diferencia <- .tolerancia + 1
  
  # utilidad Inicial (en el portafolio inicial), no se verifica que ese 
  # portafolio sea válido.
  
  utilidadActual <- wrapperUtilidad(
    .portIni
  )
  
  # se define el portafolio actual
  
  
  portafolioActual <- .portIni
  
  #  se escoge un salto inicial relativamente pequeño (este podrá crecer
  #  en la primera iteración)
  
  saltoActual <- .saltoInicial
  
  # while principal
  
  saltoActual <- 0.01
  
  while ((contador < .maxIter) & (diferencia > .tolerancia)) {
    
    print(contador)
    
    
    # fase 1 ------------------------------------------------------------------
    
    # se realiza un cálculo en el vecindario de todos los activos.
    
    # !!! definir que pasa si se pega
    
    # definir el salto
    
    # resultado lista
    
    cambiosCompletos <- cambiarPortafolioCompleto(
      ..portafolio = portafolioActual,
      ..movimiento = saltoActual 
    )
    
    # $e calcula la utilidad de cada uno de los puntos modificados
    # se sabe que en estos puntos la suma del portafolio da 1
    
    cLimites <- cambiosCompletos |> 
      sapply(\(x) sum(x[tes]) <= limiteTes) 
    
    cambiosCompletos <- cambiosCompletos[cLimites]
    
    utilidadesCompletas <- cambiosCompletos |> 
      sapply(
        wrapperUtilidad, 
        simplify = T
      )
    
    
    # se encuentra el máximo.
    
    auxCompleto <-  utilidadesCompletas |> 
      which.max()
    
    utilidadMaxComp <- utilidadesCompletas[auxCompleto] 
    
    diferenciaCompleta <- utilidadMaxComp - utilidadActual
    
    # se calculan el portafolio con la utilidad maxima 
    
    
    portafolioCMaximo <- cambiosCompletos[[auxCompleto]]
    
    
    # fase 2 ------------------------------------------------------------------
    
    # se pregunta si se está lo suficientemente cerca de un límite.
    
    if ((sum(portafolioActual[tes]) + saltoActual) >= limiteTes){
      # se esta lo suficientemente cerca del límite de TES por lo tanto toca 
      # calcular que esta pasando en la pared
      
      # en primer lugar se fuerza al portafolio actual a pegarse a la pared
      
      # subida TES
      
      distanciaLIM <- limiteTes - sum(portafolioActual[tes])
      
      # esto se puede simplificar, se deja explicito
      
      subidaTES <- 1 + distanciaLIM/sum(portafolioActual[tes])
      
      bajadaOtros <- 1 - distanciaLIM/sum(portafolioActual[-tes])
      
      portAuxTes <- portafolioActual
      
      portAuxTes[tes] <- portAuxTes[tes] * subidaTES
      
      # se reducen proporcionalmente los demás activos para que se conserve
      # que los pesos sumen 1
      
      portAuxTes[-tes] <- portAuxTes[-tes] * bajadaOtros
      
      # en este punto se prueba cuanto es la utilidad del nuevo portafolio
      
      utilidadTESaux <- wrapperUtilidad(portAuxTes)
      
      
      # a continuacion se reliza la prueba sobre el límite de TES.
      portafoliosTES <- cambiarTES(
        ..portafolio = portAuxTes, 
        ..tes = tes, 
        ..movimiento = saltoActual - distanciaLIM
      )
      
      utilidadTES <- portafoliosTES |> 
        sapply(wrapperUtilidad, simplify = T)
      
      tesAux <- utilidadTES |> 
        which.max()
      
      utilidadMaxTES <- utilidadTES[tesAux] 
      
      # se prueba cual de las dos tiene una utilidad mayor.
      # (portafolio en la frontera, o con cambios)
      
      if (utilidadMaxTES >= utilidadTESaux) {
        
        portafolioTESMaximo <- portafoliosTES[[tesAux]]
        
        
        
      } else {
        
        utilidadMaxTES <- utilidadTESaux
        
        portafolioTESMaximo <- portAuxTes
        
      }
      
      # en este punto se calcula si el portafolio maximo de tes le gana al 
      # portafolio en el que se cambiaron todos los activos.
      
      if (utilidadMaxTES >= utilidadMaxComp){
        
        # los tes le ganan a los demás activos
        
        portafolioIteracion <- portafolioTESMaximo
        utilidadIteracion <- utilidadMaxTES
        
        
      } else {
        
        # los demás activos le ganan a los tes
        
        portafolioIteracion <- portafolioCMaximo
        utilidadIteracion <- utilidadMaxComp
        
        
      }
      
      
      
    } else {
      
      # no se está cerca del límite de tes por lo tanto el máximo de la
      # iteración será de 
      
      portafolioIteracion <- portafolioCMaximo
      utilidadIteracion <- utilidadMaxComp
      
      
    }
    
    
    # se calcula la diferencia entre la utilidad anterior y la nueva utilidad
    
    # la utilidad tiene que ser al menos tan grande 
    
    if ( utilidadIteracion < utilidadActual) {
      
      # hay convergencia, se encuentra en un maximo
      
      diferencia <- 0
      
    } else {
      
      # hay aprendizaje
      
      diferencia <- utilidadIteracion - utilidadActual 
      
      portafolioActual <- portafolioIteracion
      
      utilidadActual <- utilidadIteracion
      
      # se define una nueva tasa de crecimiento:
      
      # este parámetro es importante para la velocidad de crecimiento.
      
      saltoActual <- diferencia * 10
      
    }
    
    contador <- contador + 1
    
  }
  
  print(saltoActual)
  
  if (contador == .maxIter) { 
    warning("convergencia por máximas iteraciones")
  }
  
  return(portafolioActual)
  
}
