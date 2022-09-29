# Extracción

En este directorio se encuentran scripts (de Python y de R) para extraer y procesar datos de los estados financieros de las AFP. Se realiza un web scrapping a la página de la SFC, posteriorme se extrae la información y se ingresa a R en un formato usable.

El archivo: srcrappingPUC.ipynb es el encargado de realizar el scrapping desde python. Posteriormente, unirPUC.r une todos los resultados en un archivo plano usable. Por último, el archivo: manipulacion.R es donde se puede acceder fácilmente a la información (requiere el archivo plano que se creo con el segundo script) 
