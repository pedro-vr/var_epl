#Funciones para la lectura y transformación de datos de la PL obtenida de la página web Football-Data
#Autor: Pedro Velázquez
#Fuente de datos: Football-Data

#Instalamos las librerias a utilizar
library(tidyr)

#Función para leer el archivo de stats_epl
read_stats_data <- function(ruta_file,season){
  #Transformamos la variable "season" para que se pueda leer el archivo
  season_format <- gsub('/','',season)
  #Formateamos la ruta final con la temporada indicada
  ruta_final_file <- paste0(ruta_file,'stats_epl_',season_format,'.csv')
  #Leemos el archivo
  pl_stats <- read.csv(ruta_final_file)
  
  #Del archivo final eliminamos la columna 'DIV'
  pl_stats <- pl_stats %>% select(-c('Div'))
  #Agregamos la columna "season" a la tabla
  pl_stats['season'] <- season
  
  return(pl_stats)
}