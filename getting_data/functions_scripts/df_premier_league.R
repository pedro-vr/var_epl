#Funciones para la lectura y transformación de datos de la PL obtenida de FiveThirtyEight
#Autor: Pedro Velázquez
#Fuente de datos: FiveThirtyEight

#Función para leer la data de la url de FiveThirtyEight
read_538_data <- function(url_data){
  football_matches <- read.csv(url_data,fileEncoding = 'UTF-8',skipNul = T)
  return(football_matches)
}