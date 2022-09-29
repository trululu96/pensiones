# esta función no se utiliza en el script final. En el final se utiliza una
# optimzación de gradiente restringida. Se deja acá para referencia de cómo 
# generar una optimización con grilla que respete los límites impuestos sobre
# diferentes activos.



# se requiere una funcion que se llame funcionUtilidad para que esto corra,
# debe tener como primer arugumento un portafolio y posteriormente se pueden
# poner otros argumentos nombrados.


# funcion para realizar la optimización de un portafolio
# toca tener en cuenta que esto utiliza fuerza bruta para hacerlo y que no
# es particularmente eficiente para crear los portafolios,
# está paralelizado para el momento de la evaluación de función de utilidad.

# esta función se utiliza extensivamente para crear portafolios, se debe cambiar
# cuando se agreguen activos.

# se requiere que en otro script se cree una función definida como función de
# utilidad



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

  # se normaliza el portafolio a un

  portafolioBase <- portafolioBase/sum(portafolioBase)


  return(portafolioBase)

}


optimizarPortafolio <- function(
  .matrizSigma,
  ...
){


  cores <- detectCores()

  # en principio es 11

  saltos5 <- 11

  # en principio es 26

  saltos3 <- 26

  # en principio hay 0.02

  porGrillaFina <- 0.02
 
  # funcion a evaluar, esta debe depender de un portafolio
  # debe retornar un numeric con la utilidad esperada de un portafolio.
  # se pasan todos los argumentos anónimos que que pasaron a la
  # función principal

  wrapperFuncion <- function(
    ..portafolio
  ){
    # esta funcion debe ser creada por fuera de este script
    
    resultado <- funcionUtilidad(
      ..portafolio,
      ...
    )



    return(resultado)
  }

  # funcion para crear una grilla general

  crearGrillaGeneral <- function(
      ..dimensiones,
      ..saltos # saltos para cada dimension

  ){

    # ojo, esto es muy ineficiente bienvenida sea cualquier mejora

    # se crea la grilla
    grilla  <- list()
    inicial <- seq(0,1,length.out = ..saltos)
    # se crean todas las permutaciones con reemplazo
    inicial <- permutations(
      ..saltos,
      ..dimensiones,
      inicial,
      repeats.allowed = T
      )

    # se conservan solo las que sumen 1
    resultado <- inicial[which(rowSums(inicial) == 1),]

    # se convierte en una lista

    resultado <- lapply(
      seq_len(nrow(resultado)),
      \(x) as.numeric(unname(resultado[x,])
                      )
      )

    return(resultado)

  }

  # funcion para escoger una utilidad de la lista

  maximaUtilidad <- function(
    ..lista, # esta lista contiene una utilidad esperada en cada entrada
    ..portafolios # lista con los portafolios
  ){
    ..lista <- sapply(
      ..lista,
      I
    )

    maximo <- which.max(..lista)

    return(
      list(
        portafolio = ..portafolios[[maximo]],
        UE = ..lista[maximo]
      )
    )

  }

  # índices

  grillaIndices <- crearGrillaGeneral(
   ..dimensiones = 3,
   ..saltos = saltos3
  )

  gc()

  # 351 posibles portafolios

  # se crea funcion para crear los portafolios


  grillaIndices <- lapply(
    grillaIndices,
    {\(x) vPortafolioAux(
      `MXWD INDEX` = x[1],
      `LBUSTRUU INDEX` = x[2],
      `IPRV LN Equity` = x[3]
      )
      }
    )

  # se pasa la grilla a un lapply paralelizado

  utilidadIndices <- mclapply(
    grillaIndices,
    wrapperFuncion,
    mc.cores = cores
  )

  indices <- maximaUtilidad(
    ..lista = utilidadIndices,
    ..portafolios = grillaIndices
  )[["portafolio"]]

  # se crea una grilla para los nodos de corto plazo y vista

  grillaCP <- crearGrillaGeneral(
   ..dimensiones = 3,
   ..saltos = saltos3
  )

  gc()

  # 351 posibles portafolios

  # se crea funcion para crear los portafolios


  grillaCP <- lapply(
    grillaCP,
    {\(x) vPortafolioAux(
      `Ahorros` = x[1],
      `COP1` = x[2],
      `UVR1` = x[3]
      )
      }
    )

  # se pasa la grilla a un lapply paralelizado

  utilidadCP <- mclapply(
    grillaCP,
    wrapperFuncion,
    mc.cores = cores
  )

  CP <- maximaUtilidad(
    utilidadCP,
    grillaCP
  )[["portafolio"]]

  # se crea una grilla para los COP

  grillaCOP <- crearGrillaGeneral(
    ..dimensiones = 5,
    ..saltos = saltos5
  ) |>
    lapply(
      {\(x) vPortafolioAux(
        `COP3` = x[1],
        `COP5` = x[2],
        `COP10` = x[3],
        `COP20` = x[4],
        `COP30` = x[5]
      )}
    )
  gc()

  # Se aplica a la función de utilidad a la grilla

  utilidadCOP <- mclapply(
    grillaCOP,
    wrapperFuncion,
    mc.cores = cores
  )

  # se crea una grilla para los UVR

  grillaUVR <- crearGrillaGeneral(
    ..dimensiones = 5,
    ..saltos = saltos5
  ) |>
    lapply(
      {\(x) vPortafolioAux(
        `UVR3` = x[1],
        `UVR5` = x[2],
        `UVR10` = x[3],
        `UVR20` = x[4],
        `UVR30` = x[5]
      )}
    )

  # Se aplica a la función de utilidad a la grilla

  utilidadUVR <- mclapply(
    grillaUVR,
    wrapperFuncion,
    mc.cores = cores
  )


  # en este punto se utiliza una función auxiliar para realizar una optimización
  # más fina en los puntos cercanos a el portafolio óptimo.



  crearGrillaAux <- function(
    ..portafolio,# portafolio desde el que se parte para crear la grilla fina,
    ..portafolioEW, # portafolio bajo el cual se define si se debe variar
    ..por = porGrillaFina # que tan finos son los saltos
  ){

    # se requiere un vector para cada uno de los pesos

    funcionAux <- function(..x, ..y){
      if ( ..y == 0 ){

        return( c(0) )

      }else{ # en caso que sea diferente de 0 se genera una secuencia

        inicial <- ..x - 0.1
        final <- ..x + 0.1


        secuencia <- seq(
          from = inicial,
          to = final,
          by = ..por
        )

        # se filtran los negativos y superiores a 1.


        secuencia <- secuencia[secuencia >= 0]
        secuencia <- secuencia[secuencia <= 1]


        return(secuencia)

      }
    }

    # se aplica la función

    lista <- mapply(
      funcionAux,
      as.list(..portafolio),
      as.list(..portafolioEW)
      )


    # se realiza la expansion de la grilla.
    # se tiene que filtrar por los que sumen 1 (esto no es eficiente pero es
    # mucho más fácil de programar)

    grilla <- expand.grid(lista)

    grilla <- grilla[rowSums(grilla) == 1, ]


    resultado <- lapply(
      seq_len(nrow(grilla)),
      \(x) as.numeric(unname(grilla[x,])
      )
    )

    # se retorna la grilla

    return(resultado)

  }

  # portafolioEW, este es un portafolio mock para que se puedan identificar los
  # activos en los cuales va a haber variación.

  portafolioEWCOP <- vPortafolioAux(
        `COP3` = 1,
        `COP5` = 1,
        `COP10` = 1,
        `COP20` = 1,
        `COP30` = 1
      )

  maximoCOP <- maximaUtilidad(
    ..lista = utilidadCOP,
    ..portafolios = grillaCOP
  )

  grillaFinaCOP <- crearGrillaAux(
    ..portafolio = maximoCOP[["portafolio"]],
    ..portafolioEW = portafolioEWCOP
  )

  gc()

  # se realiza la evaluación de utlidad

  utilidadFinaCOP <- mclapply(
    grillaFinaCOP,
    wrapperFuncion,
    mc.cores = cores
  )

  utilidadMaximaCOP <- maximaUtilidad(
    ..lista = utilidadFinaCOP,
    ..portafolios = grillaFinaCOP
  )

  colCOP <- utilidadMaximaCOP[["portafolio"]]

  # se realiza lo mismo para los UVR

  portafolioEWUVR <- vPortafolioAux(
        `UVR3` = 1,
        `UVR5` = 1,
        `UVR10` = 1,
        `UVR20` = 1,
        `UVR30` = 1
      )

  maximoUVR <- maximaUtilidad(
    ..lista = utilidadUVR,
    ..portafolios = grillaUVR
  )

  grillaFinaUVR <- crearGrillaAux(
    ..portafolio = maximoUVR[["portafolio"]],
    ..portafolioEW = portafolioEWUVR
  )

  gc()

  # se realiza la evaluación de utlidad

  utilidadFinaUVR <- mclapply(
    grillaFinaUVR,
    wrapperFuncion,
    mc.cores = cores
  )

  utilidadMaximaUVR <- maximaUtilidad(
    ..lista = utilidadFinaUVR,
    ..portafolios = grillaFinaUVR
  )

  colUVR <- utilidadMaximaUVR[["portafolio"]]

  # en este punto se combinan los ejercicios de todos los resultados

  # se crea una función que filtre los portafolios válidos.

  filtro <- function(..portafolios){

    # este filtro tiene que retornar los portafolios a escoger que cumplan una
    # serie de características:
    #

    nombres <- names(..portafolios[[1]])

    # cuales son tes, esto no va a servir para portafolios que solo tengan 1
    # sin embargo esto no debería tener sentido

    tes <- grep(
      "UVR|COP",
      nombres
    )


    filtroAux <- function(..portafolio){
      # esta funcion tiene que tomar un portafolio y retornar T or F dependiendo
      # si pasa el filtro

      resultado <- all(
        sum(..portafolio[tes]) <= 0.50,
        ..portafolio["Ahorros"] <= 1
      )
      return(resultado)
    }

    # se aplica el flitro a toda la lista

    vectorFiltro <- sapply(
      ..portafolios,
      filtroAux
    )

    return(vectorFiltro)

  }

  # funcion para crear los portafolios finales

  crearPortafolioFinal <- function(
    ..x
  ){
    colcap <- vPortafolioAux(`COLCAP INDEX` = 1)

    portafolio <- ..x[1] * colCOP +
      ..x[2] * colUVR +
      ..x[3] * indices +
      ..x[4] * CP +
      colcap * ..x[5]


    names(portafolio) <- names(colcap)

    return(portafolio)

  }
  

  grillaFinal <- crearGrillaGeneral(
    ..dimensiones = 5,
    ..saltos = saltos5
  )

  gc()

  # se convierte la grilla final en portafolios.

  portafoliosFinal <- grillaFinal |>
    lapply(crearPortafolioFinal)

  vectorFiltro <- filtro(portafoliosFinal)

  portafoliosFinal <- portafoliosFinal[vectorFiltro]

  grillaFinal <- grillaFinal[vectorFiltro]

  #calculo de utilidades


  utilidadesGenerales <- mclapply(
    portafoliosFinal,
    wrapperFuncion,
    mc.cores = cores
  )

  # Se requiere conocer una función

  # calculo de maxima utilidad

  maximaUtilidadGeneral <- maximaUtilidad(
    ..lista = utilidadesGenerales,
    ..portafolios = portafoliosFinal
  )

 # browser()

  maximoGeneral <- sapply(
    utilidadesGenerales,
    I
  ) |>
    which.max()

  # por último se realiza una optimización fina de el portarfolio final.

  crearGrillaFina <- function(
    ..puntoGrilla,
    ..por = porGrillaFina
  ){

    funcionAux <- function(..x){

      inicial <- ..x - 0.1
      final <- ..x + 0.1


      secuencia <- seq(
        from = inicial,
        to = final,
        by = ..por
      )

      # se filtran los negativos y superiores a 1.


      secuencia <- secuencia[secuencia >= 0]
      secuencia <- secuencia[secuencia <= 1]


      return(secuencia)

    }

    grilla <- lapply(
      as.list(..puntoGrilla),
      funcionAux
      )

    grilla <- expand.grid(grilla)

    grilla <- grilla[rowSums(grilla) == 1, ]

    grilla <- lapply(
      seq_len(nrow(grilla)),
      \(x) as.numeric(unname(grilla[x,])
      )
    )

    return(grilla)

  }

  # se crea una grilla fina:

  grillaFinaFinal <- crearGrillaFina(
    ..puntoGrilla = grillaFinal[[maximoGeneral]]
  )

  gc()


  PortafoliosFinosFinales <- grillaFinaFinal |>
    lapply(crearPortafolioFinal)

  
  # se aplica el filtro de los limites de inversion

  PortafoliosFinosFinales <- PortafoliosFinosFinales[
    filtro(PortafoliosFinosFinales)
    ]
  

  # se corre una última optimización

  utilidadFinal <- mclapply(
    PortafoliosFinosFinales,
    wrapperFuncion,
    mc.cores = cores
    )

  utilidadMaximaTotal <- maximaUtilidad(
    ..lista = utilidadFinal,
    ..portafolios = PortafoliosFinosFinales
  )

  return(utilidadMaximaTotal[["portafolio"]])

}

