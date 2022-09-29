# archivo con las funciones para manipuar el PUC de las AFP

WrapPUC <- function(
  .cuentas,
  .titulos
){
  
  # funcion para leer los archivos y extrar las cuentas relevantes
  
  readfile <- function(
    .name,
    .auxCuentas = .cuentas , 
    .auxTitulos = .titulos, 
    .out = "cuentas"
    ){
    print(.name)
    

    
    suppressMessages(xlsData <- read_xls(.name, sheet = 1) |> 
      data.frame() )
    
    nombre <- xlsData |> 
      {\(x) x[1:7, ]}() 
    
    # esta funcion sirve para cambiar los nombres de las columnas en caso que se
    # tenga un formato viejo ya que pueden tener totales que no son útiles
    
    AuxFun <- function(z, n){
      
      resultado <- z[n,] |> 
        {\(x) x[-2][-1]}()
      
      total <- grepl("total", resultado[length(resultado)], ignore.case = T)
      
      if(total){
       resultado <- resultado[-length(resultado)] 
      }
      
      resultado <- c("cuenta", resultado)
      
      # total indica la presencia de una columna que sea el total
      
      return(list(resultado, total))
    }
    
    # se segmentan para conservar los nombres. 
    
    AuxCol <- xlsData[,3] |> 
      stri_trans_general(id = "Latin-ASCII") |> 
      {\(x)gsub("(*.*.-)|(S[.]A[.])|(AFPC)", "", x)}() |> 
      tolower() |> 
      {\(x) grepl("proteccion", x, ignore.case = T)}()
      
    col_ini <- which(AuxCol)[1]
    
    if(is.na(nombre[3,2])){
      # se tiene un formato viejo
      
      # se eliminan las columnas que no son relevantes (segunda y ultima)
      # lista donde la primera entrada son las columnas y la segunda es 
      # si existe una columna de total que se debe eliminar
      
      columnas <- AuxFun(xlsData, col_ini)
      
      
      
      if(columnas[[2]]){
        xlsData <- xlsData[, -ncol(xlsData)]
      }
      
      
      
      # se asignan los nombres de las columnas 
      
      # se cambian los números 
      
      
      nombre <- nombre[3, 1] |> 
        tolower()
      
      nombre <- sub("fecha de informe:", "", nombre) |>
        trimws() |> 
        as.Date(format="%d/%m/%Y")
      
      
      
    } else{ 
      nombre <- nombre[3,2] |>
        as.numeric() |> 
        as.Date(origin = "1899-12-30")
      
      # lista donde la primera entrada son las columnas y la segunda es 
      # si existe una columna de total que se debe eliminar
      
      columnas <- AuxFun(xlsData, col_ini)
      
      
      
      if(columnas[[2]]){
        xlsData <- xlsData[, -ncol(xlsData)]
      }
      
      
      
        
    }
    
    xlsData <- xlsData[(col_ini + 1):nrow(xlsData), -2]
    
    rownames(xlsData) <- NULL 
    colnames(xlsData) <- columnas[[1]] |> 
      stri_trans_general(id = "Latin-ASCII") |> 
      tolower() |> 
      {\(x)gsub("(*.*.-)|(s[.]a[.])|(afpc)|(s[.]a)", "", x)}() |> 
      trimws()
    
    # pueden existir todavia columnas vacias entonces se eliminan
    if( sum(colnames(xlsData) == "na")) {
      
      xlsData <- xlsData[,- which(colnames(xlsData) == "na")]
      
    }
    suppressWarnings( xlsData <- xlsData |> 
      apply(MARGIN = c(1,2), {\(x)gsub("\\\\","0",x)}) |> 
      apply(MARGIN = c(1,2), as.numeric)
    )
    
    
    
    rownames(xlsData) <- xlsData[,1] |> 
      as.integer() |> 
      as.character()
    xlsData <- xlsData[,-1] 
    
    # se realiza la filtracion 
    
    .auxCuentas <- .auxCuentas |> 
      as.integer() |> 
      as.character()
    
    xlsData <- xlsData[.auxCuentas, ]
    
    if(.out == "titulos"){
      rownames(xlsData) <- titulos
    }
    return(list(xlsData, nombre))
  }
  
  completo <- as.list(list.files("./data/final/")) |> 
    lapply(\(x) paste0("./data/final/", x))
  
  completo <- lapply(completo, readfile) 
  
  
  # se agregan nombres y se organizan los arrays por fechas
  
  nombres <- c()
  
  for(i in completo){
    nombres <- c(nombres, i[[2]])
  }
  
  
  completoAux <- completo
  
  nombres <- as.Date(nombres,origin = "1970-01-01")
  
  names(completo) <- nombres
  
  nombres <- as.character(nombres)
  organizado <- list()
  
  for( i in nombres){
    organizado[[i]] <- completo[[i]][[1]]
  }
  
  organizado <- organizado[sort(nombres)]
  
  
  return(organizado)
}


extraerPUC <- function(
  .x, 
  .lista, 
  .direccion = "./data/IPC.xlsx" ,
  .Abase = 2021
    ){
  # .x es la cuenta a extraer (as.character),
  # .lista es un objeto que proviene de WrapPUC
  # .direccion datos del IPC
  # .Abase año base para los pesos
  # extrae la entrada para cada una de las fechas llevada a pesos constantes del
  # año base
  
  
  lista <- lapply(.lista, {\(z) z[.x,]}) |> 
    sapply(\(z) z) |> 
    t()
  
  
   if(as.numeric(.x) >= 400000){ 
     # la cuenta es con flujos entonces se tiene que mensualizar. Por lo que
     # se tienen que restar los anteriores meses del mismo año.
     
     # OJO esto solo funciona si se empieza en enero.
     
     if(which(months.Date(as.Date(rownames(lista))[1]) == month.name)[1] > 1){
       # tire un error
       stop("Esta vaina no esta organizada desde enero")
     }
     
     
     listaAux <- lista
     # se encuentran los diciembres de cada año (son el punto inicial de cada
     # una de las restas)
     
     diciembres <- 1:nrow(lista) %% 12 == 0 
     
     listaAux[diciembres, ] <- 0 
     
     # se crea un lagg de uno para restar.
     
     listaAux <- listaAux[-nrow(listaAux), ]
     
     listaAux <- rbind(rep(0,ncol(listaAux)), listaAux)
     
     rownames(listaAux) <- NULL
     
     # se realiza la corrección
     lista <- lista - listaAux
   }
  
  IPC <- read_xlsx(.direccion) |> 
    data.frame()
  
  
  IPC <- IPC |> 
    mutate( 
      fecha = `fecha` |> 
        paste0("01") |> 
        as.Date(format="%Y%m%d")
     ) |> 
    filter(
      month(`fecha`) == 12
    )
  
  base <- IPC[["indice"]][which(year(IPC[["fecha"]]) == .Abase)[1]]
  
  IPC <- IPC |> 
    mutate(
     indice = base/`indice`,
     fecha = year(`fecha`)
    ) 
  
  indice <- IPC[["indice"]] 
  
  names(indice) <- IPC[["fecha"]] |> 
    as.character()
  
  aux <- rownames(lista) |> 
    as.Date() |> 
    year() |> 
    as.character() |> 
    sapply(\(x) indice[[x]]) |> 
    unname() |> 
    rep(ncol(lista)) |> 
    matrix(ncol = ncol(lista))
  
  resultado <- aux * lista
  
  return(resultado)
}

 

