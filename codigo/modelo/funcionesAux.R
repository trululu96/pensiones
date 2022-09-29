# estas funciones se requieren para realizar la optimizacion completa
# URF.

# funcion para definir un portafolio benchmark basado en un portafolio 
# que salga de una optimización de función de utilidad de los afiliados/
# entes territoriales.
# nótese que depende de la composición de los asset classes.

#' crear portafolio benchmark 
#' 
#'
#' @param .optimo portafolio benchmark óptimo según el equivalente de certeza de
#' los afiliados. Esto proviene del modelo de los afiliados.
#'
#' @returnq portafolio en el formato correcto (nótese que depende )
#' @export
#'
#' @examples

cPortafolioBenchmark <- function(.optimo){
  
  # se crea un portafolio auxiliar
  
  portafolioBenchmark <- vPortafolioAux(
    Ahorros = 1
  )*0
  
  portafolioBenchmark[names(.optimo)] <- .optimo 
  
  # se normaliza por si las moscas
  
  portafolioBenchmark <- portafolioBenchmark/sum(portafolioBenchmark)
  
  return(portafolioBenchmark)
}


# función de utilidad de las AFP y la respectiva función para calcular
# el equivalente de certeza

funcionUtilidadEntidades <- function(.r, .aversionRiesgo){ 
  
  limite <- 0.99
  
  # se determina la utilidad en el límite
  
  util_base  <- ((1 - limite)^(1 - .aversionRiesgo) - 1) /
    (1 - .aversionRiesgo)
  
  # se determina la pendiente utilizando la derivada en el límite
  
  pendiente  <- (1 - limite)^(1 - .aversionRiesgo - 1) 
  
  # Se convierte en lineal por debajo del límite
  
  utilidad  <- (((1 + .r)^(1 - .aversionRiesgo)) - 1)/(1 - .aversionRiesgo)
  
  
  utilidad[.r <= -limite] <- pendiente*(.r[.r <= -limite] + limite) + util_base
  
  
  return(utilidad)
  
}

equivalenteEntidades <- function(.u, .aversionRiesgo){
  
  aversionRiesgo <- 1 - .aversionRiesgo
  
  equivalente <- (.u * aversionRiesgo + 1)^(1 / aversionRiesgo) - 1
  
  return(equivalente)
}



#' wrapper de la función de utilidad de las AFP 
#'
#' @param ..portafolio portafolio sobre el que se haya la utilidad esperada
#' @param ... otros argumentos que se deben pasar a la funcion de utilidad 
#'
#' @return retorna el equivalente de certeza para este portafolio
#' @export
#'
#' @examples
funcionUtilidad <- function(
  ..portafolio, 
  ...
  ){
  
  # se extraen los valores de la lista de argumentos
  
  lista <- list(...)
  
  vParametrosRegulacion <- lista[["vParametrosRegulacion"]]
  
  vParametrosAFP <- lista[["vParametrosAFP"]]
  
  portafolioBenchmark <- lista[["portafolioBenchmark"]]
  
  datos <- lista[["datos"]]
  
  equivalenteEntidades <- lista[["equivalenteEntidades"]]
  
  periodoActual <- lista[["periodoActual"]]
  
  # nótese que el valor inicial del portafolio está fijado en 100.
  
  # se aplica la funcion de utilidad esperada que requiere los demás argumentos
  # que se pusieron en la lista.
  
  res <- utilidadEsperada(
    .portafolio = ..portafolio,
    .datos = datos,
    .periodoActual = periodoActual, 
    .parametrosAFP = vParametrosAFP,
    .parametrosRegulacion = vParametrosRegulacion,
    .portafolioBenchmark = portafolioBenchmark,
    .valorInicialPortafolio = 100,
    .portafolioEstabilizacion = portafolioBenchmark
  )
  
  # se calcula la utilidad esperada
  
  utilidadInicial <- res[["utilidad"]][["UE"]]
  
  # se calcula el equivelente de certeza que es sobre lo que se optimiza
  
  equivalente  <- utilidadInicial |>
    equivalenteEntidades()
  
  
  return(equivalente)
  
}


#' funcion para optimizar en una regulación
#'
#' @param x vector de dos entradas donde x[1] =  b y x[2] = alpha
#' @param ... otros parametros que se requieren para calcular la función 
#' de utilidad
#'
#' @return portafolio óptimo que maximiza la utilidad esperada de las entidades 
#' (AFP)
#' @export
#'
#' @examples
optimizarRegulacion <- function(
  x, 
  ...){
  # x son los parámetros sobre los que se optimiza.
  # x[1] = b
  # x[2] = alpha
  
  lista <- list(...)
  
  vParametrosAFP <- lista[["vParametrosAFP"]]
  
  portafolioBenchmark <- lista[["portafolioBenchmark"]]
  
  datos <- lista[["datos"]]
  
  equivalenteEntidades <- lista[["equivalenteEntidades"]]
  
  portIni <- lista[["portafolioInicial"]]
  
  periodoActual <- lista[["periodoActual"]]
  
  limiteTes <- lista[["limiteTes"]]
  
  
  # Parametros de regulación iniciales (se incluyen las parametrizaciones de
  # x[1] y x[2]) 
  
  vParametrosRegulacion <- parametrosRegulacion(
    .RE = RE(
      .parametro = 0.99
      ),
    .RM = RM(
      .variableMedicion = "nominal",
      .b = x[1], 
      .frecuenciaMedicion = 3,
      .pMedicion = 12
    ),
    .CD = CD(
      .pago = x[2],
      .portafolioAlternativo = vPortafolioAux(Ahorros = 1),
      .pPago = 0.8
    )
  )
  
  # se optimiza la función de utilidad para este salto, notese que el portafolio
  # inicial que en este caso es el benchmark debe de cumplir con todas la 
  # restricción de límite de tes, de lo contrario la función no encontrará
  # portafolios que cumplan con la restricción.
  
  portafolioResultado <- optimizarGradiente(
    .tolerancia = 0.00001,
    .maxIter = 30,
    .portIni = portIni,
    .funcionUE = funcionUtilidad,
    .saltoInicial = 0.01,
    .limiteTes = limiteTes,
    datos = datos, 
    vParametrosRegulacion = vParametrosRegulacion,
    vParametrosAFP = vParametrosAFP,
    portafolioBenchmark = portafolioBenchmark,
    equivalenteEntidades = equivalenteEntidades,
    periodoActual = periodoActual
  )
  
  # se calcula la utlidad esperada del portafolio de resultado
  
  resultado <- utilidadEsperada(
    .portafolio = portafolioResultado,
    .datos = datos,
    .periodoActual = periodoActual,
    .parametrosAFP = vParametrosAFP,
    .parametrosRegulacion = vParametrosRegulacion,
    .portafolioBenchmark = portafolioBenchmark,
    .valorInicialPortafolio = 100,
    .portafolioEstabilizacion = portafolioBenchmark
  )
  
  # se retorna una lista de las cosas que pueden resultar útiles para realizar
  # gráficas.
  
  return(
    list(
      portafolio = portafolioResultado,
      optimizacion = resultado,
      regulacionAFP = vParametrosAFP,
      regulacion = vParametrosRegulacion,
      equivalente = equivalenteEntidades(resultado[["utilidad"]][["UE"]])
    )
  )
  
}
 

#' funcion de utilidad de entes
#' 
#' el límite de la función a partir de la cual los entes se vuelven neutrales al
#' riesgo está escrito dentro de la función y no se toma como parámetro, en caso
#' de querer cambiarla se debe cambiar directamente dentro de la función. 
#'
#' @param .r vector de TIR de los entes
#' @param .aversionRiesgo aversion al riesgo de los entes
#'
#' @return vector con las utilidades de los entes
#' @export
#'
#' @examples
funcionUtilidadEntes <-  function(
  .r,
  .aversionRiesgo
  ){ 
  
  limite <- 0.2
  
  # se determina la utilidad en el límite
  
  util_base  <- ((1 - limite)^(1 - .aversionRiesgo) - 1)/(1 - .aversionRiesgo)
  
  # se determina la pendiente utilizando la derivada en el límite
  
  pendiente  <- (1 - limite)^(1 - .aversionRiesgo - 1) 
  
  # Se convierte en lineal por debajo del límite
  
  utilidad  <- (((1 + .r)^(1 - .aversionRiesgo)) - 1)/(1 - .aversionRiesgo)
  
  
  utilidad[.r <= -limite] <- pendiente*(.r[.r <= -limite] + limite) + util_base
  
  
  return(utilidad)
  
}

# La solución analítica del equivalente de certeza requiere varios "ifs" para 
# que sea consistente con el límite. Por lo anterior se sugiere dejarlo como
# una función que encuentre la raíz. En caso que se tenga que llamar múltiples
# veces esta función (por ejemplo para una optimización sobre esta parte) 
# se sugiere programar los if's de manera explícita 

#' Cálculo del equivalente de certeza de los entes
#' 
#' Notese que esta función no está vectorizada. 
#'
#' @param .UE utilidad esperada
#' @param .aversionRiesgo aversion al riesgo de los entes
#'
#' @return equivalente de certeza de los entes de certeza para una utilidad esperada.
#' @export
#'
#' @examples
equivalenteEntes <- function(
  .UE, 
  .aversionRiesgo
  ){
  
  # funcion auxiliar, la raiz de esta función será igual a cero. 
  
  auxFUN <- function(..r){
    funcionUtilidadEntes(..r, .aversionRiesgo) - .UE
  }
  
  resultado <- uniroot(auxFUN, c(-1,1))[["root"]]
  
  return(resultado)
  
}

# funcion para procesar los datos al final

#' funcion para realizar un procesamiento de los datos
#'
#' @param .optimos 
#' @param .grilla 
#' @param .valorUP 
#' @param .aversionEntes 
#'
#' @return
#' @export
#'
#' @examples
procesarDatos <- function(
  .optimos, 
  .grilla, 
  .valorUP,
  .aversionEntes
  ){
  
  # armar dataframe con los portafolios
  
  
  portafolios <- sapply(
    .optimos, 
    \(x) x[["portafolio"]]
    ) |> 
    t() 
  
  portafoliosDepurado <- matrix(0,nrow = nrow(portafolios), ncol = 8) |> 
    data.frame()
  
  colnames(portafoliosDepurado) <- c(
    colnames(portafolios)[1:4], 
    "COP CP",
    "COP LP",
    "UVR CP",
    "UVR LP"
  )
  
  portafoliosDepurado[,1:4] <- portafolios[,1:4]
  
  portafoliosDepurado[,"COP CP"] <- portafolios[,c("COP1", "COP3", "COP5")] |>
    rowSums()
  
  portafoliosDepurado[,"UVR CP"] <- portafolios[,c("UVR1", "UVR3", "UVR5")] |>
    rowSums()
  
  
  # auxiliares para sumar los diferentes TES de largo plazo
  
  auxLPUVR <- colnames(portafolios)[grep("UVR", colnames(portafolios))]  
    
  filtroAux <- auxLPUVR |> 
    str_sub(-2) |> 
    {\(x) gsub("[a-z]|[A-Z]", "0", x)}() |> 
    as.numeric() |> 
    {\(x) x >= 10}()
  
  auxLPUVR <- auxLPUVR[filtroAux]
  
  portafoliosDepurado[,"UVR LP"] <- portafolios[,auxLPUVR] |>
    rowSums()
  
  auxLPCOP <- colnames(portafolios)[grep("COP", colnames(portafolios))]  
    
  filtroAux <- auxLPCOP |> 
    str_sub(-2) |> 
    {\(x) gsub("[a-z]|[A-Z]", "0", x)}() |> 
    as.numeric() |> 
    {\(x) x >= 10}()
  
  auxLPCOP <- auxLPCOP[filtroAux]
  
  portafoliosDepurado[,"COP LP"] <- portafolios[,auxLPCOP] |>
    rowSums()
  
  
  portafoliosDepurado <- round(portafoliosDepurado * 100, 2)
  
  equivalentes <- sapply(.optimos, \(x) x[["equivalente"]])
  
  RE <- sapply(.optimos , \(x) x[["optimizacion"]][["reserva"]])
  reservaPreMaximo <- sapply(
    .optimos , 
    \(x) x[["optimizacion"]][["reservaPreMaximo"]]
    )
  
  TIRprom <- sapply(
    .optimos , 
    \(x) x[["optimizacion"]][["utilidad"]][["TIR"]] |> mean()
    ) 
  
  markers <- .grilla |>  
    sapply(c) |> 
    t() |> 
    data.frame()
  
  colnames(markers) <- c("b", "alpha")
  
  
  reservaE <- RE |> 
    round(3) |> 
    {\(x) x*100}()
  
  reservaPre <- reservaPreMaximo |> 
    round(3) |> 
    {\(x) x*100}()
  
  vInicial <- lapply(.optimos, 
                     \(x){
                       resultado <- x[["optimizacion"]][["flujos"]]["vPortafolio", 2,]
                     } )
  
  vFinal <- lapply(.optimos, 
                   \(x){
                     resultado <- x[["optimizacion"]][["flujos"]]["vPortafolio", 14,]
                   } )
  
  
  
  UPini <- .valorUP[[1]]
  
  UPfini <- .valorUP[[2]]
  
  vInicial <- lapply(vInicial,
                     \(x) x/UPini
  )
  
  vFinal <- lapply(vFinal,
                   \(x) x/UPfini
  )
  
  utilidadMedia <- mapply(
    \(ini, fin) funcionUtilidadEntes(fin/ini - 1, .aversionEntes) |> 
      mean(), 
    ini = vInicial, 
    fin = vFinal
  )
  
  
  grafica <- cbind(markers, equivalentes) |> 
    mutate(
      b = as.factor(b)
    ) |>
    cbind(reservaE, reservaPre) |> 
    data.frame()
  
  
  equivalentesEntes <- sapply(
    utilidadMedia, 
    equivalenteEntes,
    .aversionRiesgo = .aversionEntes
    )
  
  grafica <- grafica |> 
    cbind(equivalentesEntes)
  
  portafoliosDepurado <- portafoliosDepurado |> 
    cbind(grafica)
  
  return(
    list(
      portafolio = portafoliosDepurado,
      grafica = grafica
    )
  )
}
# funcion para procesar datos.
# esta función va a realizar completamente la optimización de una regulación.



#' funcion para optimizar una regulacion
#' 
#' Tener en cuenta que el portafolio óptimo tiene que cumplir todas las 
#' restricciones y además debe tener algún porcentaje en TES.
#'
#' @param .datos cubo con datos correctos
#' @param .aversionRiesgoEntes aversión al riesgo de los entes/afiliados 
#' @param .aversionRiesgoEntidades aversión al riesgo de las fiudciarias/AFP
#' @param .pathPortafolioOptimo path con el RDS del portafolio óptimo
#' @param .pathUP path con el RDS de los UPS 
#' @param .periodoActual cual columna de los datos representa los últimos
#' datos históricos 
#' @param .limiteTes Qué % del portafolio puede invertirse en tes
#' @param .alphas vector con las diferentes opciones de alpha
#' @param .betas vector con las diferentes opciones de betas
#'
#' @return lista con los resultados de la optimización para las combinaciones de
#' alpha y beta
#' @export
#'
#' @examples
datosCompletos <- function(
  .datos,
  .aversionRiesgoEntes,
  .aversionRiesgoEntidades,
  .pathPortafolioOptimo,
  .pathUP,
  .periodoActual,
  .limiteTes,
  .alphas,
  .betas
){
  
  
  # en se leen los archivos planos que se requieren para la optimización
  portafolioOptimo <- readRDS(.pathPortafolioOptimo) |> 
    cPortafolioBenchmark()
  
  valorUP <- readRDS(.pathUP)
  
  
  # sobre esta grilla se va a realizar la optimización inicial del portafolio
  grillaInicial  <- expand.grid(.betas, .alphas) |>
    {\(x) lapply(seq_len(nrow(x)), function(i){
      return(unname(x[i,]) |> as.numeric())
    })}()
  
  # el orden final de esta grilla tiene que ser: beta, alpha.
  
  # más abajo se muestra como se cambia el portafolio inicial cada vez que
  # re inicia el beta.
  
  # wrapper para interiorizar la aversion al riesgo de los entes (AFP)
  
  wrapperUtilidadEntidades <- function(..r){
    
    resultado <- funcionUtilidadEntidades(
      .r = ..r, 
      .aversionRiesgo = .aversionRiesgoEntidades
    )
    
    return(resultado)
    
  }
  
  # wrapper para interiorizar la aversión al riesgo de los entes ()
  
  wrapperEquivalenteEntidades <- function(..u){
    
    resultado <- equivalenteEntidades(
      .u = ..u, 
      .aversionRiesgo = .aversionRiesgoEntidades
    )
    
    return(resultado)
    
  }
  
  
  # definición de los parámetros de las AFP
  
  vParametrosAFP  <- parametrosAFP(
    .funcionUtilidad = wrapperUtilidadEntidades
  )
  
  segmento <- list()
  
  # se definie un portafolio Inicial
  # tener en cuenta que esto toma el portafolio anterior en caso que alpha
  # obtenga
  
  portafolioInicial <- portafolioOptimo
  
  for (i in 1:length(grillaInicial)) {
    if ((length(.alphas) > 1) & ((i - 1) %% length(.betas) == 0)) {
      
      # en caso que haya más de un alpha y que se tome más de un beta entonces
      # se procede a reiniciar el portafolio cada vez que se cambia de beta.
      # tener en cuenta que se tiene que iterar primero sobre los betas 
      # y luego sobre los alphas.
      
      portafolioInicial <- portafolioOptimo
    }
    
    res <- optimizarRegulacion(
      grillaInicial[[i]],
      datos = .datos, 
      vParametrosAFP = vParametrosAFP,
      portafolioBenchmark = portafolioOptimo, 
      equivalenteEntidades = wrapperEquivalenteEntidades,
      portafolioInicial = portafolioInicial,
      periodoActual = .periodoActual,
      limiteTes = .limiteTes
    )
    
    
    # en caso que no se cambie de alpha, se utiliza como portafolio inicial
    # el portafolio anterior, esto garantiza que la solución será al menos tan
    # buena como la anterior.
    
    portafolioInicial <- res[["portafolio"]]
    
    
    segmento[[i]] <- res
  }
  
  # en este punto termina la optimizacion y se realizan los diferentes ajustes
  # a los datos para que estos sean utilizables.
  
  # retorna una lista con los portafolios ya procesados y un dataframe que 
  # permite hacer gráficas.
  
  resultadoInicial <- procesarDatos(
    .optimos = segmento,
    .grilla = grillaInicial,
    .valorUP = valorUP,
    .aversionEntes = .aversionRiesgoEntes
  )
  
  # las optimizaciones están guardadas en el objeto segmento
  
  resultadoInicial[["optimizaciones"]] <- segmento
  
  
  return(resultadoInicial)
  
}



#' funcion para realizar una gráfica del equivalente de certeza de las entidades
#'
#' @param .grafica objeto proveniente de datosCompleto[["grafica"]]
#' @param .ylab Título del eje y
#' @param .titulo Título de la gráfica
#'
#' @return
#' @export
#'
#' @examples
graficaEntidades <- function(
  .grafica, 
  .ylab, 
  .titulo
  ){
  
  .grafica[["b"]] <- .grafica[["b"]] |> 
    as.character() |> 
    as.numeric() |> 
    round(3) |> 
    {\(x) paste0(x*100,"%")}() |> 
    as.factor()
  
  .grafica |> 
    ggplot(aes(x = alpha, y = equivalentes)) +
    geom_point(aes(color = b),  size = 4) + 
    scale_y_continuous(labels = scales::label_percent()) +
    scale_x_continuous(labels = scales::label_percent()) + 
    theme(text = element_text(size = 20)) +
    ylab(.ylab) + 
    ggtitle(.titulo)
  
}



# se genera una gráfica para el equivalente de los entes

graficaEntes <- function(.grafica, .ylab, .titulo, .drift = 0){
  
  .grafica[["equivalentesEntes"]] <- .grafica[["equivalentesEntes"]] - .drift
  
  .grafica[["equivalentesEntes"]] <- .grafica[["equivalentesEntes"]] |> 
    round(3)
  
  .grafica |> 
    ggplot(aes(x = alpha, y = equivalentesEntes)) +
    geom_point(aes(color = b),  size = 4) + 
    scale_y_continuous(labels = scales::label_percent()) +
    scale_x_continuous(labels = scales::label_percent()) + 
    theme(text = element_text(size = 20)) +
    ylab(.ylab) + 
    ggtitle(.titulo)
  
}

calculoTrackingError <- function(
  .datosCompletos,
  .datos,
  .aversionRiesgoEntidades,
  .pathPortafolioOptimo,
  .periodoActual
){
  # se calcula el valor final del portafolio si se tomase el portafolio
  # optimo como portafolio (se sigue perfectamente al benchmark)
  
  portafolioBenchmark <- readRDS(.pathPortafolioOptimo) |> 
    cPortafolioBenchmark()
  
  referencia <- utilidadEsperada(
    .portafolio = portafolioBenchmark,
    .datos = .datos,
    .periodoActual = .periodoActual,
    .parametrosAFP = .datosCompletos[["optimizaciones"]][[1]][["regulacionAFP"]],
    .parametrosRegulacion = .datosCompletos[["optimizaciones"]][[1]][["regulacion"]],
    .portafolioBenchmark = portafolioBenchmark,
    .valorInicialPortafolio = 100,
    .portafolioEstabilizacion = portafolioBenchmark
  )
  
  valorInicial <- referencia[["flujos"]]["vPortafolio", 2, ]
  valorFinal <- referencia[["flujos"]]["vPortafolio", 2 + 12, ]
  
  retornosBase <- valorFinal/valorInicial - 1
  
  funcionAux <- function(..x){
    valorInicialAux <- ..x[["optimizacion"]][["flujos"]]["vPortafolio", 2, ]
    valorFinalAux <- ..x[["optimizacion"]][["flujos"]]["vPortafolio", 2 + 12, ]
    
    retornosAux <- valorFinalAux/valorInicialAux - 1
    
    resultado <- retornosAux - retornosBase
    
    resultado <- resultado |> 
      sd()
    
    return(resultado)
  }
  
  
  trackingCompleto <- sapply(.datosCompletos[["optimizaciones"]],funcionAux)
  
  trackingCompleto <- round(trackingCompleto * 100, 2)
  
  .datosCompletos[["grafica"]][["TE"]] <- trackingCompleto
  
  
  return(.datosCompletos)
  
}


# Gráfica entes para la optimización fina de beta.


graficaEntesBeta <- function(.grafica, .ylab, .titulo, .drift = 0){
  # en este punto la columna que corresponde a b está en forma de factor
  .grafica[["b"]] <- .grafica[["b"]] |> 
    as.character() |> 
    as.numeric() 
  
  .grafica[["equivalentesEntes"]] <- .grafica[["equivalentesEntes"]] - .drift
  .grafica |> 
    ggplot(aes(x = b, y = equivalentesEntes)) +
    geom_line(size = 2) + 
    scale_y_continuous(labels = scales::label_percent()) +
    scale_x_continuous(labels = scales::label_percent()) + 
    theme(text = element_text(size = 20)) +
    ylab(.ylab) + 
    ggtitle(.titulo)
  
}


graficarTE <- function(.grafica, .ylab, .titulo, .drift = 0){
  .grafica[["b"]] <- .grafica[["b"]] |> 
    as.character() |> 
    as.numeric()
  .grafica[["TE"]] <- .grafica[["TE"]]/100 - .drift
  
  .grafica |> 
    ggplot(aes(x = b, y = TE)) +
    geom_point(size = 4) + 
    scale_y_continuous(labels = scales::label_percent()) +
    scale_x_continuous(labels = scales::label_percent()) + 
    theme(text = element_text(size = 20)) +
    ylab(.ylab) + 
    ggtitle(.titulo)
  
}


graficarRE <- function(
  .grafica,
  .ylab,
  .titulo
){
  
  .grafica[["b"]] <- .grafica[["b"]] |> 
    as.character() |> 
    as.numeric()
  
  .grafica[["reservaPre"]] <- .grafica[["reservaPre"]]/100 
  
  .grafica |> 
    ggplot(aes(x = b, y = reservaPre)) +
    geom_point(size = 4) + 
    scale_y_continuous(labels = scales::label_percent()) +
    scale_x_continuous(labels = scales::label_percent()) + 
    theme(text = element_text(size = 20)) +
    ylab(.ylab) + 
    ggtitle(.titulo)
}

# funcion para graficar todos los alphas desde la función  betas desde la gráfica

graficarUnAlpha <- function(
    .grafica,
    .titulo,
    .ylab,
    .alpha,
    .drift = 0 
){ 
  # se toma un beta determinado, se filtra y posteriormente se grafica solo para
  # ese.
  
  filtro <- .grafica[["alpha"]] == .alpha
  
  .grafica <- .grafica[filtro,] 
  
  .grafica |> 
    graficaEntesBeta(
      .ylab = .ylab,
      .titulo = .titulo,
      .drift = .drift
    )
}

graficarTEAlpha <- function(
    .grafica,
    .titulo,
    .ylab,
    .alpha,
    .drift = 0 
){ 
  # se toma un beta determinado, se filtra y posteriormente se grafica solo para
  # ese.
  
  filtro <- .grafica[["alpha"]] == .alpha
  
  .grafica <- .grafica[filtro,] 
  
  .grafica |> 
    graficarTE(
      .ylab = .ylab,
      .titulo = .titulo,
      .drift = .drift
    )
}



# funcion para calcular los ingresos como porcentaje de los AUM de cada fondo


calcularIngresos <- function(
    .datosCompletos
){
  
  # esta funcion calcula los ingresos promedios para una optimizacion
  
  
  funcionAux <- function(..x){
    ingresos <- ..x[["optimizacion"]][["flujos"]]["fCD", , ] |> 
      colSums()
    
    
    # valor promedio del fondo
    
    vFondo <- ..x[["optimizacion"]][["flujos"]]["vPortafolio", , ] |> 
      colMeans()
    
    
    # a los ingresos se le debe sumar el flujo del último periodo (cuanto vale
    # la reserva de comision por desempeño)
    
    #auxiliar de número de meses
    
    nMeses <- ..x[["optimizacion"]][["flujos"]] |> 
      ncol()
    
    ingresosFinales <- ..x[["optimizacion"]][["flujos"]]["vCD",nMeses,]  
    
    
    # se agrega a los ingresos
    
    ingresos <- ingresos + ingresosFinales
    
    
    # se saca cuanto se gana en promedio sobre el valor del fondo
    
    
    resultado <- (ingresos/vFondo) |> 
      mean()
    
    return(resultado)
    
  }
  
  ingresos <- sapply(.datosCompletos[["optimizaciones"]],funcionAux)
  
  ingresos <- round(ingresos * 100, 4)
  
  .datosCompletos[["grafica"]][["ingresos"]] <- ingresos
  
  return(.datosCompletos)
  
}



