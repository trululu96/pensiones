# funciones de cálculo de la utilidad esperada de una AFP que administre 
# recursos del público.

## limites para la creacion de variables


#' Valorar portafolios dado un cubo de simulaciones
#'
#' @param .simulaciones un cubo de simulaciones que contiene los precios
#' proyectados de los activos
#' @param ... un número arbitrario de portafolios para ser valorados en cada
#' momento de las simulaciones
#'
#' @return array con la valoración de los portafolios en cada momento 
#' el orden es (portafolio, meses, simulaciones)
#' @export
#'
#' @examples
#' 

valorarPortafolios <- function(
  .simulaciones,
  .valorUnidad,
  ...
){ 

  # se realiza la valoración de una unidad de portafolio, la unidad está 
  # definida como el valor de una unidad de portafolio que se compró en el
  # momento 0 y posteriormente es vendida cada mes y re balanceado.
   
  # orden cubo cubo[activos, meses, simulaciones]

  # nótese que los precios de los activos deben estar en la misma moneda.
  
  aux <- list(...)

  # se escoge un valor inicial para la unidad del portafolio
  # en este caso será .valorUnidad en la moneda correcta. 

  # valor del portafolio. 
  
  # funcion para convertir las proporciones de los portafolios en cantidades
  # de activos. Se calcula cual es la distribución del portafolio en cada 
  # momento

  aux <- lapply(
    aux, 
    \(z) { 
        array(matrix(
          z,
          nrow = dim(.simulaciones)[1],
          ncol = dim(.simulaciones)[2]
        ), 
        dim = dim(.simulaciones) 
        ) |>
        {\(x) (.valorUnidad * x / .simulaciones) }()
    }
    )


  # se calcula cuanto vale un portafolio de un numerario en el periodo 
  # siguiente respecto al periodo en el que se compró.

  resultado <- lapply(
    aux,
    \(z) { 

      res <- (z[,-dim(z)[2],]*.simulaciones[,-1,]) |>
        colSums()

      res  <- rbind(rep(.valorUnidad), res) |>
        {\(x)x/.valorUnidad}() |>
        apply(2, cumprod) |>
        {\(x)x * .valorUnidad}() # este paso se podría incluir en pasos
                                 # anteriores pero se deja explícito 
                                 # por facilidad en el entendimiento 

      return(res)
      }
    ) |>  
    sapply(I, simplify = "array") |> 
    aperm(c(3,1,2)) # se requiere transponer el cubo
  
  # se corrige el nombre de las columnas.

  dimnames(resultado)[2] <- dimnames(.simulaciones)[2]

  # se retorna el valor de un portafolio de un numerario en el periodo 0 en 
  # cada momento.

  return(resultado)
  
}

#' Separar datos
#'
#' @param .datos cubo con los datos historicos y las simulaciones. 
#' @param .periodoActual posicion en la segunda dimension del cubo que
#' representa el punto en el cual empiezan las simulaciones
#' @param .parametrosRegulacion Lista con la especificación regulatoria.
#'
#' @return deja en el ambiente de la función dos objetos con las tasas de cambio
#' a pesos y con las simulaciones de los precios de los activos (en pesos)
#' @export
#'
#' @examples
unpackDatos <- function(
  .datos,
  .periodoActual
){

  resultado <- list()


  
  
  # se filtran los datos para que solo se tome lo que es necesario.

  corteMonedas <- which(
    dimnames(.datos)[[1]] == "nominal"
  )[1]
  
  # nominal es una matriz que contiene 1 en todas las entradas 
  # (tasa de cambio de pesos a pesos)

  
  # se mantiene solo desde el último dato que es histórico
  # periodo actual debe ser el primer mes de simulaciones.
  
  
  .datos <- .datos[,(.periodoActual - 1):dim(.datos)[2],]



  # se extraen los datos de las simulaciones.
  
  resultado[[".simulaciones"]] <- .datos[1:(corteMonedas - 1),,]

  # se extraen los datos de las monedas.

  resultado[[".monedas"]] <- .datos[corteMonedas:dim(.datos)[1],,]

  # se agregan los objetos al ambiente de la fución. 

  # tener en cuenta que es sys.frame(-1) para que queden los objetos creados
  # dentro de una función. En caso que se llame esto desde el ambiente global
  # quedarán como objetos globales, esto no es recomendable ya que quedarán como
  # objetos ocultos (punto al frente del nombre del objeto).
  
  
  list2env(
    resultado,
    envir = sys.frame(-1)
  )
  
  # recordatorio. Se tiene que mantener el orden de los activos consistente
  # alrededor de todo el código. 
  
}



#' convertirMoneda 
#' 
#' Esta funcion convierte a la moneda correcta un cubo de simulaciones
#'
#' @param .simulaciones cubo de simulaciones (activos, meses, sim)
#' @param .monedas cubo con la conversion de las monedas
#' @param .variableMedicion cual moneda es a la que se quiere convertir, tener
#' en cuenta que en caso de ser nominal no se hará nada
#'
#' @return un cubo de dimensiones iguales con los precios en las monedas 
#' correctas.
#' 
#' @export
#'
#' @examples
convertirMoneda <- function(
  .simulaciones,
  .monedas,
  .variableMedicion
){
  
  aux <- .monedas[.variableMedicion,, ]
  
  # en este punto se divide el valor por el precio de la moneda correcta en
  # cada una de las simulaciones (revisar esto con datos, creo que esta bien)
  
  .simulaciones <- .simulaciones/(
    replicate(dim(.simulaciones)[1], aux) |> 
      aperm(c(3, 1, 2))
    )
  
  
  return(.simulaciones)
  
  
}



#' cálculo de rentabilidad para un periodo 
#' 
#' Se calcula la rentabilidad anualizada desde el periodo inicial 
#' hasta cada uno de los meses posteriores. 
#'
#' @param .valorPortafolios cubo con la valoración de los portafolios.
#'
#' @return array del mismo tamaño que .valorPortafolios con las 
#' rentabilidades.
#' @export
#'
#' @examples
calcularRentabilidad <- function(
  .valorPortafolios
){
  # se calculan la rentabilidad en ventanas acordes a .parametrosRegulacion
  # para cada uno de los portafolios que se encuentren en .valorPortafolios
  nMeses <- dim(.valorPortafolios)[2]


  # se calcula la rentabilidad 

  valorInicial <- .valorPortafolios[,1,, drop = F] |>
    array(
      dim = c(
        dim(.valorPortafolios)[1],
        dim(.valorPortafolios)[2] - 1,
        dim(.valorPortafolios)[3]
        ),
        )
    
  # !!! esto debería de funcionar

  valorFinal <-  .valorPortafolios[,-1,, drop = F]

  rentabilidad <- (valorFinal/valorInicial)
  
  # en este punto se tiene que anualizar la rentabilidad de los portafolios
  
  # se calcula un vector auxiliar con los meses
  
  vecAux <- 1:(nMeses - 1)
  
  # se realiza un array con las dimensiones correctas para realizar una 
  # anualización de las rentabilidades.
  
  arrayAux <- replicate(
    dim(.valorPortafolios)[3], 
    replicate(
      dim(.valorPortafolios)[1],
      vecAux
    )  |> t()
    )
  
  # se realiza el proceso de anualización, se deja en términos de rentabilidad.
  
  rentabilidad <- rentabilidad^(12/arrayAux) - 1


  # se agregan las dimensiones que se requieren para el array de resultados

  resultado <- array(
    data = 0,
    dim = dim(.valorPortafolios)
  )
  
  # se tiene que omitir el primer mes (se deja en cero) ya que en este no se 
  # esta midiendo rentabilidad

  
  resultado[,2:nMeses,] <- rentabilidad
  

  dimnames(resultado)  <- dimnames(.valorPortafolios)

  return(resultado)

}



#' Cálculo de la rentabilidad mínima.
#' 
#' Calcula la rentabilidad mínima para cada mes, según los parámetros de la 
#' rentabilidad mínima
#'
#' @param .historicoRentabilidad Cubo con el histórico de rentabilidad para los
#' diferentes portafolios.
#' @param .parametrosRegulacionRM Parámetros que definen la rentabilidad mínima.
#'
#' @return matriz con la rentabilidad mínima para cada 
#' @export
#'
#' @examples
#' 
rentabilidadMinima <- function(
  .historicoRentabilidad,
  .parametrosRegulacionRM
){
  
  # En esta funcion se pueden agregar características del portafolio benchmark
  # por fuera del alpha y beta. Por esto se requieren lo parámetros
  # de la regulación de la rentabilidad mínima. En caso que se requieran más 
  # parámetros se pueden agregar en este objeto.
  
  
  # se calcula la rentabilidad mínima para cada uno de los meses.

  rho <- .historicoRentabilidad["portafolioBenchmark",,,drop = T]
  
  # rho será entendido como la rentabilidad del portafolio benchmark que tiene
  # un componente de portafolio de pares y una de un portafolio definido.


  return(rho) 

}



calcularFlujoRM <- function(
  .historicoRM,
  .valorPortafolios,
  .parametrosRegulacionCD,
  .parametrosRegulacionRM,
  .parametrosRegulacionRE,
  .valorInicialPortafolio
) {
  
  # funcion eficiente para calcular la rentabilidad minima de un fondo

  # construcción del array de flujos

  resultado <- array(
    data = 0,
    dim = c(
      15,
      dim(.valorPortafolios)[2],
      dim(.valorPortafolios)[3]
    )
  )

  dimnames(resultado) <- list(
    c(
      "fRM", # flujo rentabilidad minima.
      "fCD", # flujo comisión por desempeño.
      "vPortafolio", # cuanto vale el portafolio
      "uPortafolio", # cuantas unidades hay en el portafolio
      "vRE", # valor de la reserva de estabilizacion
      "uRE", # unidades de la reserva de estabilizacion
      "pagoRM", # cuanto se ha pagado por RM (contablemente)
      "vCD", # cuanto vale la reserva de la comision por desempeño.
      "uCD", # cuantas unidades hay en la reserva de comision por desempeño.
      "pagoCD", # cuanto se ha pagado por CD (contablemente)
      "rAux", # rentabilidad del portafolio benchmark
      "vUnidad", # valor de la unidad del portafolio.
      "rMin", #rentabilidad mínima 
      "vTeoricoCD", # valor teórico del portafolio en caso que rente igual que la CD
      "vRM" 
      ),
    dimnames(.valorPortafolios)[[2]]
  )
  
  
  # !!!!!!!!! 
  
  # en este punto se pueden inyectar gastos u otros ingresos en caso que estos
  # sean necesarios para la modelación específica. La forma más fácil de hacerlo
  # es al final de esta funcion de tal forma que no afecten los demás cálculos
  # de la rentabilidad mínima. 
  
  
  resultado["rAux",,] <- .historicoRM 
  
  

  # valor de la unidad del portafolio:

  resultado["vUnidad",,] <- .valorPortafolios[ "portafolio",,,drop = T ]


  # se crea una lista con los meses iniciales en los que se tiene que calcular
  # rentabilidad mínima y comision por desempeño. 

  # se realiza el supuesto de que se van a pagar en el mismo momento.

  # se extraen los parametros de la comisión por desempeño. 

  cdUmbral <- .parametrosRegulacionCD[["umbral"]]
  cdPago <- .parametrosRegulacionCD[["pago"]]
  inicial <- .parametrosRegulacionRM[["pMedicion"]] + 2
  
  # ojo la frecuencia de medicion tiene que ser <= a pMedicion 
  
  fMedicion <- .parametrosRegulacionRM[["frecuenciaMedicion"]] 
  
  # en este punto se calcula cuanto es el valor de la reserva de estabilizacion
  # este valor ignora completamente la comisión por desempeño. Por lo tanto 
  # supone que esta es igual a cero en todo momento.
  
  resultado["rMin",,] <- pmin(
    .historicoRM - .parametrosRegulacionRM[["b"]]
  )
  
  # se crea un array auxiliar para calcular los flujos de rentabilidad mínima
  
  
  
  auxArr <- array(
    0,
    dim = c(2, dim(.valorPortafolios)[2:3])
  )
  
  
  dimnames(auxArr) <- list(
    c("RM", "unidadesRE")
    
  )
  
  meses <- replicate(
    dim(resultado)[3], 
    0:(ncol(resultado) - 1)
  )
  
  
  
  vTeoricoRM <- .valorInicialPortafolio*((resultado["rMin",,] + 1 )^(meses/12))
  

  # unidades iniciales del portafolio


  auxValorReal  <- .valorInicialPortafolio/resultado["vUnidad",1,]

  
  vRealRM <-  auxValorReal*resultado["vUnidad",,] 
  
  auxArr["RM",,] <- pmax(0, vTeoricoRM - vRealRM)
  
  # se convierte esto a unidades del portafolio RM
  
  auxArr["unidadesRE",,] <- auxArr["RM",,]/
    .valorPortafolios["portafolioEstabilizacion",,]
  
  # en este punto se descartan los meses previos a la primera medición de RM
  
  # por lo pronto no se define ningún periodo inicial.
  
   auxArr <- auxArr[,inicial:(dim(auxArr)[2]),]
  
  
  # !!! Hard Coded !!!
  # Esto solo tiene en cuenta los primeros meses del año.
  # acá tiene que ir 1:3
  
  maximoArr <- apply(
    auxArr["unidadesRE",1:3 , ], 
    MARGIN = 2,
    max
  )
  
  reserva <- quantile(
    maximoArr,
    .parametrosRegulacionRE[["parametro"]]
  )[1]

  # se realiza una implementación del cVar

  reserva <- maximoArr[maximoArr >= reserva] |>
      mean()

  
  reserva <- reserva * .valorPortafolios["portafolioEstabilizacion",1,1] 
  
  reserva <- reserva/.valorInicialPortafolio
  
  #!!! ----- piso reserva estabilizacion -----
  
  # en este punto se pone el piso de la reserva de estabilización se garantiza
  # que esta nunca sea menor a este valor
  # esto se podría cambiar como un parámetro, sin embargo en esta versión se
  # conserva hard coded en las funciones de utilidad esperada
  
  
  reservaPreMaximo <- reserva

  reserva <- pmax(
    0.003,
    reserva
  )
  
  # se debe ingresar el valor inicial del portafolio y de las unidades 
  # iniciales
  
  resultado["uPortafolio",1:(inicial - fMedicion),] <- .valorInicialPortafolio /
    resultado["vUnidad",
    1,
    ]
  
  # se actualizan cuales son las unidades del portafolio
  
  resultado["vPortafolio",1:(inicial - fMedicion),] <- resultado[
    "uPortafolio",
    1:(inicial - fMedicion),
    ]*resultado["vUnidad", 1:(inicial - fMedicion),] 
  
  
  
  # se actualiza la reserva de estabilización tanto en unidades como en valor
  
  resultado["uRE",1:(inicial - fMedicion),] <- (reserva *
    .valorInicialPortafolio)/
    .valorPortafolios["portafolioEstabilizacion",1,1]
  
  resultado["vRE",1:(inicial - fMedicion),] <- resultado[
    "uRE",
    1:(inicial - fMedicion),
    ] * .valorPortafolios[
      "portafolioEstabilizacion",
      1:(inicial - fMedicion),
    ]

  
  # lista completa de meses en la cual se tiene que realizar la medición de 
  # RM y CD.
  
  mesesRM <- seq.int(from = inicial,
    to = dim(.valorPortafolios)[2],
    by = fMedicion 
    )  |> 
    as.list()
  
  # se crear una lista completa para poder realizar un reduce

  listaCompleta  <-  c(
    list(resultado),
    mesesRM
  ) 

  # funcion para calcular la rentabilidd mínima para un mes en particular
  # ..ar será el array con los datos
  # ..m será el mes (posición de la segunda dimensión del array) a la cual 
  # se le calculará la rentabilidad mínima y la comisión de desempeño.
  
  # primero se realiza una funcion auxiliar para replicar un vector y 
  # convertirlo en un array de las dimensiones adecuadas para reemplazar arrays
  # mismas dimensiones de ..arr
  
  fReplicar <- function(..vec){
    resultado <- replicate(
      fMedicion - 1,
      ..vec
    ) |>
      t()
    return(resultado)
  }
  

  funcionAux <-  function(
    ..arr,
    ..m
  ){
    
    # rango sobre el que se van a actualizar los datos
    
    rangoAct <- (..m - fMedicion + 1):(..m - 1)
    
    # último periodo con actualización
    
    pAnterior <- (..m - fMedicion)
    
    
    # en primer lugar se actualizan los valores:
    
    # uPortafolio 
    ..arr["uPortafolio", rangoAct,] <- ..arr["uPortafolio", pAnterior,] |>
      fReplicar()

    # vPortafolio se valora el portafolio

    ..arr["vPortafolio", rangoAct, ] <- ..arr["uPortafolio", rangoAct,] *
      .valorPortafolios["portafolio", rangoAct, ]

    # uRE unidades de reserva de estabilizacion en el momento

    ..arr["uRE", rangoAct,] <- ..arr["uRE", pAnterior,] |>
      fReplicar()

    # vRE

    ..arr["vRE", rangoAct,] <- ..arr["uRE", rangoAct,]*
      .valorPortafolios["portafolioEstabilizacion", rangoAct,]

    # uCD unidades invertidas en una cuenta de ahorros

    ..arr["uCD", rangoAct,] <- ..arr["uCD", pAnterior,] |>
      fReplicar()

    # vCD

    ..arr["vCD", rangoAct,] <- ..arr["uCD", rangoAct,]*
      .valorPortafolios["portafolioAlternativo",rangoAct,]


    # pagoCD que valor de la CD se ha pagado contablemente
    # se supone que no existen pagos intermedios entre los periodos de medición
    # de RM


    ..arr["pagoCD", rangoAct,] <- ..arr["pagoCD", pAnterior,] |>
      fReplicar()

    # pagoRM
    # se supone que no existen pagos intermedios entre los periodos de medición
    # de RM

    ..arr["pagoRM", rangoAct,] <- ..arr["pagoRM", pAnterior,] |>
      fReplicar()
    
    
    # en este punto se revisa cuanto vale el portafolio para determinar el pago
    # de CD o de RM
    
    uPortafolio <- ..arr["uPortafolio", ..m - 1,] 
    
    vPortafolio <- uPortafolio*.valorPortafolios["portafolio", ..m, ]
    
    # comparacion del valor para determinar RM
    # !!! pararle bolas a esto, si es - 1
    
    vRM <- (..arr["rMin", ..m, ] + 1)^((..m - 1)/12)*.valorInicialPortafolio
    
    ..arr["vRM", ..m, ] <- vRM
    # calculo de rentabilidad mínima
    
    RM <- pmax(
      vRM - vPortafolio,
      0
    )
    
    # se paga rentabilidad mínima si hay fondos para hacerlo.
    # se devuelven fondos solo si:
    # - ya no se encuentra en zona de RM
    # - ya se había pagado RM
    
    # flujo para pagar al fondo
    
    # se actualiza un valro provisional del portafolio.
    
    ..arr["vRE", ..m,] <- ..arr["uRE", (..m - 1), ]*
        .valorPortafolios["portafolioEstabilizacion",..m, ] 
    
    # solo se puede pagar al fondo tanta plata como haya en la reserva de 
    # estabilización.
    
    fRM <- pmin(
      RM, 
      ..arr["vRE", ..m,]
      )
    
    # se actualiza el valor
    ..arr["fRM", ..m, ] <- fRM
    
    # cuanto se ha pagado de rentabilidad mínima
    
    pagoRM <- ..arr["pagoRM", (..m - 1),] + fRM
    
    # flujo para pagar a la reserva de estabilizacion (en caso que se tengan 
    # que devolver fondos)
    
    
    
    devolucionRM <- pmin(
      pagoRM,
      vPortafolio - vRM
    )

    
    # Se debe poner el piso en 0, en caso que el número sea negativo quiere
    # decir que el valor del portafolio es inferior al de rentabilidad mínima
    # por lo tanto se pagó rentabilidad mínima más arriba.
    
    devolucionRM <- pmax(
      0,
      devolucionRM
    )
    
    # se actualiza cuanto se ha pagado
    
    pagoRM <- pagoRM - devolucionRM
    
    # se actualiza el valor del pago RM en el array general
    
    ..arr["pagoRM", ..m, ] <- pagoRM
    
    # se actualiza el valor de la RE con el pago de RM y de CD.
    
    ..arr["vRE",..m,] <- ..arr["vRE", ..m, ] - fRM + devolucionRM
    
    # se calculan las nuevas unidades
    
    ..arr["uRE", ..m,] <- ..arr["vRE", ..m, ]/
      .valorPortafolios["portafolioEstabilizacion", ..m, ]
    
    # se actuliza el valor del fondo 
    
    vPortafolio <- vPortafolio + fRM - devolucionRM
    
    # se realiza el cálculo de la comisión por desempeño.
    
    # rentabilidad para la comisión por desempeño

    cDesempeno <- (1 + ..arr["rAux", ..m,] + cdUmbral)
    
    # se calcula el valor teorico del portafolio si se cumple el umbral de la 
    # CD
    
    vTeoricoCD <- .valorInicialPortafolio*( cDesempeno ^ ((..m - 1)/12))
    
    ..arr["vTeoricoCD", ..m,] <- vTeoricoCD
    
    # se determina cuanto se debio de haber pagado hasta ese momento.
    # esta parte va a ser inutil cuando se paga RM, pero sospecho que por
    # velocidad es más fácil hacer todo el proceso que hacer nsimulaciones ifs
    
    fCDTeorico <- pmax(
      0,
      vPortafolio - vTeoricoCD
    )*.parametrosRegulacionCD[["pago"]]
    
    # se determina cuanto se ha pagado hasta el momento y si hay que pagar más.
    # esto es lo que debe salir del fondo para pagar
    
    
    # se determina cuanto sale del fondo y cuanto sale de la reserva de CD
    
    
    uCD <- ..arr["uCD", (..m - 1), ]
    
    vCD <- uCD * .valorPortafolios["portafolioAlternativo", ..m, ]
    
    # cuanto toca pagar a las administradoras
    
    # aca se cambia el regulatorio por un número que sea consistente con
    # el apetito de riesgo 
    
        # .parametrosRegulacionCD[["pPago"]] - ..arr["pagoCD", (..m - 1), ]
    fCD <- pmax(
      0,
      fCDTeorico*
        .parametrosRegulacionCD[["pPago"]] - ..arr["pagoCD", (..m - 1), ]
    )
    
    # se asigna el pago 
    
    ..arr["fCD", ..m,] <- fCD
    
    # se asigna cuanto se ha pagado en total
    
    ..arr["pagoCD", ..m, ] <- ..arr["pagoCD", (..m - 1), ] + fCD
    
    # ahora se determina cuanto tiene que haber en la reserva de CD
    
    # !!! yo creo que esto está bien
    
    stockCD <- pmax(
      fCDTeorico - ..arr["pagoCD", ..m, ],
      0
    )
    
    # en este punto se actualizan todos los datos:
    
    ..arr["vCD", ..m,] <- stockCD
    
    ..arr["uCD", ..m,] <- stockCD/
      .valorPortafolios["portafolioAlternativo", ..m, ]
    
    # se actualiza el valor del portafolio y las unidades del mismo.
    
    vPortafolio <- vPortafolio + vCD - stockCD - fCD
    
    ..arr["vPortafolio", ..m, ] <- vPortafolio
    
    ..arr["uPortafolio", ..m, ] <- vPortafolio/
      .valorPortafolios["portafolio", ..m, ]
    
    
    return(..arr)

  }


  # se calcula la existencia de la rentabilidad mínima para todos los meses

  resultado <- Reduce(
    funcionAux,
    listaCompleta
    )
  
  
  # se retornan los flujos, la reserva de estabilización y la reserva antes de
  # ponerle un piso
  
  return(
    list(
    flujos = resultado,
    reserva = reserva,
    reservaPreMaximo = reservaPreMaximo
    )
  ) 
  

}


# se calcula la utilidad esperada para la AFP de un portafolio

# se escribe una función eficiente para encontrar la TIR de los portafolios


#' Calcular TIR 
#'
#' @param .flujos matriz en la cual cada columna es una serie de flujos
#'
#' @return vector de ncol(.flujos) con la TIR en cada uno de los casos
#' @export
#'
#' @examples
calcularTIR  <- function(
  .flujos
){
  
  # TIR para 1 serie de flujos

  # se requiere una funcion que calcule el valor presente neto
  nfv <- function(
    ..r,
    ..cf
  ){
    t <- seq(along = ..cf) - 1
    
    t <- rev(t)
    
    resultado <- sum(..cf * ((1+..r)^t))
    
    return(resultado)
  }

  # funcion para calcular la TIR 
  # en caso que haya un error se retorna NA.


  funAux <- function(
    ..flujo
  ){
    
    # la tir no está definida en -1 por lo tanto se produce un error
    # en caso que este sea el resultado, entonces se retorna -1

    resultado <- tryCatch({
      
      auxProv  <- uniroot(nfv, c(-0.9999, 200), ..cf = ..flujo)[["root"]]
      
      # se realiza la anualización del resultado
      
      
      (1 + auxProv)^12 - 1
    },
    
    error = function(cond){
      
      return(-1)
      
    }
      
    )

    return(resultado)

  }

  # se realiza el calculo para toda la matriz 
  
  # (filas = meses, col = simulaciones)

  resultado <- apply(
    .flujos,
    2,
    funAux
    )
  
  return(resultado)
  
}


#' utilidadPortafolio
#' 
#' Funcion para calcular la utilidad de cada uno de las entradas del vector de
#' tires y posteriormente su utilidad esperada
#'
#' @param .flujoRM array producto de la función calcularFlujoRM contiene los 
#' flujos de un portafolio/fondo que tiene en cuenta la rentabilidad mínima
#' @param .parametrosAFP parametros regulatorios de una fiduciaria
#' @param .parametrosRegulacion parametros de un entorno regulatorio dado
#'
#' @return lista con la utilidad esperada y con la TIRes que producen esta
#' utilidad
#' @export
#'
#' @examples
utilidadPortafolio <- function(
  .flujoRM,
  .parametrosAFP,
  .parametrosRegulacion
){


  positivos <- .flujoRM["fCD", , ] 

  # se calcula la TIR de cada una de las simulaciones
  
  auxTIR <- positivos

  # se pone la reserva de estabilizacion en el primer periodo como capital 
  # inicial

  auxTIR[1,] <- -.flujoRM["vRE",1, ]
  
  # se agrega el valor final del portafolio de reserva de estabilizacion
  # se agrega el valor final de la reserva de comisión por desempeño 
  
  # auxiliar para cambiar el método de corrida.
  
  
  #### fin auxiliar
  

  auxTIR[nrow(auxTIR),] <- auxTIR[nrow(auxTIR),] + 
    .flujoRM["vRE",ncol(.flujoRM),] + .flujoRM["vCD",ncol(.flujoRM),]
  
  auxTIR <- calcularTIR(auxTIR)
  
  # se calcula la utilidad promedio

  utilidad <- .parametrosAFP[["funcionUtilidad"]](
    auxTIR
  )  |> 
    mean()

  return(
    list(
      UE = utilidad,
      TIR = auxTIR
    )
    )

}



############### Aplicación de la función ###############

#' utilidadEsperada 
#' esta función calcula la utilidad esperada de una AFP/fiduciaria dado un 
#' entorno regulatorio
#'
#' @param .portafolio un portafolio en el cual la AFP/fiduciaria inveritiría 
#' los recursos de los entes territoriales. Este tiene que ser un vector 
#' nombrado y que su suma sea 1.
#' @param .datos cubo con los precios en pesos de los activos del portafolio,
#' ademas de la tasa de cambio a las moneda en la cual se requiera la TIR de las
#' AFP.
#' @param .periodoActualultima columna de los datos que tiene datos no simulados
#' @param .parametrosAFP parámetros que definen las características de una 
#' fiduciaria
#' @param .parametrosRegulacion parámetros que definen la regulación actual
#' @param .portafolioBenchmark portafolio sobre el cual se mide
#' @param .valorInicialPortafolio numero arbitrario de cuanto vale el portafolio
#' al principio (esto no afecta los resultados, solo la escala)
#' @param .portafolioEstabilizacion  portafolio en el que se puede invertir la
#' reserva de estabilizacion
#'
#' @return lista con la utilidad esperada, nivel de reserva de estabilizacion, y
#' los flujos de un portafolio y una fiduciaria, dado un ambiente regulatorio
#' 
#' @export
#'
#' @examples
 
utilidadEsperada <- function(
  .portafolio,
  .datos,
  .periodoActual,
  .parametrosAFP,
  .parametrosRegulacion, 
  .portafolioBenchmark,
  .valorInicialPortafolio,
  .portafolioEstabilizacion
){
  # esta funcion crea los objetos que se requieren para esta funcion
  # estos son
  # .simulaciones
  # .simulacionesAfiliados
  # .monedas
  
  unpackDatos(
    .datos,
    .periodoActual
  )
  
  
  # se pasa el valor inicial del fondo a la moneda correcta esto implica que
  # las primeros meses son deterministicos (provienen de la historia)

  .valorInicialPortafolio <- .valorInicialPortafolio/
  .monedas[.parametrosRegulacion[["RM"]][["variableMedicion"]],1,1, drop = T]

  # se determina el valor numerario de las unidades del fondo. 
  # en este caso es un millón, este número no tiene incidencia sobre los 
  # resultados

  valorUnidad  <- 1e6/.monedas[
    .parametrosRegulacion[["RM"]][["variableMedicion"]],
    1, 
    1
    ]

  
  
  # las simulaciones se dejan en la moneda correcta
  
  .simulaciones <- convertirMoneda(
    .simulaciones,
    .monedas,
    .parametrosRegulacion[["RM"]][["variableMedicion"]]
  )
  
  
  # Se calcula el valor de la unidad de cada uno de los portafolios para cada
  # uno de los meses de analisis, esto incluye el portafolio del sistema
  # agregacion de cada uno de los portafolios.
  
  valorPortafolios <- valorarPortafolios(
    .simulaciones,
    valorUnidad,
    portafolio = .portafolio,
    portafolioBenchmark = .portafolioBenchmark,
    ### !!! creo qeu esto va a fallar
    portafolioAlternativo = (
      .parametrosRegulacion[["CD"]][["portafolioAlternativo"]]
      ),
    portafolioEstabilizacion = .portafolioEstabilizacion
  ) 
  
  
  # se calcula la rentabilidad de los portafolios de comparacion

  historicoRentabilidad <- calcularRentabilidad(
    valorPortafolios
  )
  
  
  # se calcula la rentabilidad minima segun los parametros.
  
  historicoRM <- rentabilidadMinima(
    historicoRentabilidad,
    .parametrosRegulacion[["RM"]]
  )

  
  # se calcula el flujo de rentabilidad minima
  
  # esta es la parte más demorada de la función
  
   resRM <- calcularFlujoRM(
    historicoRM,
    valorPortafolios,
    .parametrosRegulacion[["CD"]],
    .parametrosRegulacion[["RM"]],
    .parametrosRegulacion[["RE"]],
    .valorInicialPortafolio
  )
   
   
  # se calcula la utilidad esperada
  
  resultadoUtilidad  <- utilidadPortafolio(
    resRM[["flujos"]],
    .parametrosAFP,
    .parametrosRegulacion
  )
  
  
  retornar <- list(
    reserva = resRM[["reserva"]],
    utilidad = resultadoUtilidad,
    flujos = resRM[["flujos"]],
    reservaPreMaximo = resRM[["reservaPreMaximo"]]
  )

  return(retornar)
  
}
