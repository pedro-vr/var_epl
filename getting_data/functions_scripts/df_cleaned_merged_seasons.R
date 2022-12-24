#Funciones para la lectura y transformación de datos de la PL obtenida de GitHub: Fantasy-Premier-League
#Autor: Pedro Velázquez
#Fuente de datos: GitHub

#Instalamos las librerias a utilizar
library(tidyr)

#Función para obtener la data acerca de las temporadas de la PL
get_pl_merged_seasons <- function(ruta_file){
  #Creamos la ruta final del archivo
  ruta_merged_seasons <- paste0(ruta_file,'cleaned_merged_seasons.csv')
  #Leemos el archivo
  pl_merged_seasons <- read.csv(ruta_merged_seasons)
  
  #Eliminamos las columnas que no necesitamos
  pl_merged_seasons <- pl_merged_seasons %>% select(-c('X','opponent_team','fixture','selected'))
  #Reenombramos algunas columnas
  pl_merged_seasons <- pl_merged_seasons %>% rename(season = season_x, team1 = team_x, id_player = element)
  
  return(pl_merged_seasons)
}