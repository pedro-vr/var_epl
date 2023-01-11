#Funciones para la lectura y transformación de datos de la PL obtenida de GitHub: Fantasy-Premier-League
#Autor: Pedro Velázquez
#Fuente de datos: GitHub

#Instalamos las librerias a utilizar
library(tidyr)

#Función para leer la data acerca de los GW de ls PL
get_pl_merged_gw <- function(ruta_file,season){
  #Modificamos la variable season para la ruta final
  sub_season1 <- substr(season,1,2)
  sub_season2 <- substr(season,4,5)
  season_format <- paste0(sub_season1,sub_season2)
  #Creamos la ruta final del archivo
  ruta_merged_gw <- paste0(ruta_file,'merged_gw_',season_format,'.csv')
  
  #Leemos el archivo
  df_merged_gw <- read.csv(ruta_merged_gw)
  
  #Creamos la columna de temporada
  df_merged_gw['season'] <- season
  
  return(df_merged_gw)
}

#Funcion para remover los numeros en un string
remove_numbers <- function(name_string){
  #verificamos si dentro del nombre se tiene un numero
  has_numbers <- grepl("[[:digit:]]", name_string)
  #si tiene numero, entonces lo removemos
  if(has_numbers){
    #obtenemos las posiciones del char '_'
    pos_underscore <- unlist(gregexpr('_',name_string))
    #sabemos que el numero viene despues del ultimo '_'
    #obtenemos la posicion del ultimo '_'
    last_underscore <- pos_underscore[length(pos_underscore)]
    #sacamos el substr del nombre ya sin numero
    nombre_final <- substr(name_string,1,last_underscore-1)
  } else 
    nombre_final <- name_string
  
  return(nombre_final)
}

#Función para agregar columna del nombre del equipo local
set_local_team_name <- function(df_merged_gw,teams_catalog){
  #Del catálogo de equipos nos quedamos solo con las columnas de interés
  teams_catalog <- teams_catalog %>% select(.,c('id','season','team_name'))
  #Del nuevo catálogo modificamos la columna "season" para que coincida en el join
  teams_catalog$season <- sapply(teams_catalog$season,as.character) %>% sapply(.,substr,3,length(.)) %>% sapply(.,gsub,pattern="-",replacement="/")
  
  #Hacemos el cruce del catálogo con la tabla del merged_gw
  merged_gw <- df_merged_gw %>% left_join(teams_catalog,by = c('season','element'='id'))
  #De la nueva tabla eliminamos la columna anterior "team"
  merged_gw <- merged_gw %>% select(.,-c('team'))
  
  return(merged_gw)
}

#Función para agregar la columna del nombre del equipo contricante
set_opponent_team_name <- function(df_merged_gw,teams_catalog){
  #Del catálogo de equipos modificamos la columna "season" para que coincida en el join
  teams_catalog$season <- sapply(teams_catalog$season,as.character) %>% sapply(.,substr,3,length(.)) %>% sapply(.,gsub,pattern="-",replacement="/")
  #Removemos duplicados del catálogo sin id del equipo
  teams_catalog <- teams_catalog %>% select(.,-c('id')) %>% distinct()
  
  #Hacemos el cruce del catálogo con la tabla merged_gw
  merged_gw_formatted <- df_merged_gw %>% left_join(teams_catalog,by=c('season','opponent_team' = 'team'))
  #De la nueva tabla eliminamos la columna donde viene el código del equipo y reenombramos la nueva columna con el nombre completo
  merged_gw_formatted <- merged_gw_formatted %>% select(.,-c('opponent_team')) %>% rename(opponent_team = team_name.y, team_name = team_name.x)
  
  return(merged_gw_formatted)
}