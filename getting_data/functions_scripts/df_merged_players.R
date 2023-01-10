#Funciones para la lectura y transformación de datos de la PL obtenida de GitHub: Fantasy-Premier-League
#Autor: Pedro Velázquez
#Fuente de datos: GitHub

#Instalamos las librerias a utilizar
library(tidyr)

#Función para obtener la data acerca de los jugadores de la PL
get_pl_merged_players <- function(ruta_file,season){
  #Modificamos la variable season para la ruta final
  sub_season1 <- substr(season,1,2)
  sub_season2 <- substr(season,4,5)
  season_format <- paste0(sub_season1,sub_season2)
  #Creamos la ruta final del archivo
  ruta_merged_players <- paste0(ruta_file,'cleaned_players_',season_format,'.csv')
  
  #Leemos el archivo
  df_merged_players <- read.csv(ruta_merged_players)
  
  #Creamos la columna de temporada
  df_merged_players['season'] <- season
  
  return(df_merged_players)
}

#Función para agregar columna de posición del jugador
set_player_position <- function(df_merged_players,seasons_catalog){
  #Del catálogo entero nos quedamos solo con las variables de interés
  seasons_catalog <- seasons_catalog %>% select(.,c('name','season','id_player','position'))
  #Aplicamos formato a la columna de temporada para que coincida en el join
  seasons_catalog$season <- seasons_catalog$season %>% sapply(.,gsub,pattern='-',replacement='/') %>% sapply(.,substr,start=3,stop=7)
  
  #Hacemos el join con el df de jugadores
  df_merged_players <- df_merged_players %>% left_join(seasons_catalog,by = c('season','name'))
  #Ya que se hizo el join nos aseguramos de siempre quedarnos con el valor no nulo de la posicón del jugador
  #nos quedamos solo con una columna de posición
  df_merged_players['position_name'] <- if_else(is.na(df_merged_players$element_type),df_merged_players$position,df_merged_players$element_type)
  #borramos las anteriores dos columnas de posicion
  df_merged_players <- df_merged_players %>% select(.,-c('element_type','position'))
  
  return(df_merged_players)
}

#Función para agregar columna de nombre del equipo local/visitante
set_team_name <- function(df_merged_players,teams_catalog){
  #Del catálogo de equipos, nos quedamos solo con las columnas necesarias
  teams_catalog <- teams_catalog %>% select(.,-c('team'))
  #Aplicamos formato a la columna de temporada para que coincida en el join
  teams_catalog$season <- teams_catalog$season %>% sapply(.,gsub,pattern='-',replacement='/') %>% sapply(.,substr,start=3,stop=7)
  
  #Hacemos el join con el df de jugadores
  df_merged_players <- df_merged_players %>% left_join(teams_catalog,by = c('season','id_player' = 'id'))
  
  return(df_merged_players)
}