#Funciones que se utilizan para más de un proceso de creación de tabla de datos
#Autor: Pedro Velázquez

#Función para verificar si un equipo pertenece al Big Six
is_big_six <- function(team_name){
  big_six_teams <- c('arsenal','chelsea','liverpool','manchester city','manchester united','tottenham hotspur')
  if(tolower(team_name) %in% big_six_teams)
    big_six <- 1
  else 
    big_six <- 0
  
  big_six
}

#Función para aplicar formato a la variable "season" y quede con formato Y1/Y2
#donde Y1 = últimos dos dígitos del año de la primera parte de la temporada
#Y2 = últimos dos dígitos del año de la segunda parte de la temporada
season_format <- function(season){
  season_comp_1 <- substr(season,3,4)
  season_comp_2 <- as.character(as.integer(season_comp_1) + 1)
  season_format <- paste(season_comp_1,season_comp_2,sep = "/")
  
  return(season_format)
}

#Función para obtener la ruta en donde guardaremos las tablas finales
get_tables_directory <- function(){
  ruta <- '/Users/pedrovela/Documents/Git_repos/var_epl/tables/'
  
  return(ruta)
}

#Función para obtener la ruta de la info raw
get_raw_directory <- function(){
  ruta_raw <- '/Users/pedrovela/Documents/Git_repos/var_epl/getting_data/raw_data/'
  
  return(ruta_raw)
}

#Función para sustituir el valor de un registro de una columna por otro
sub_value_df <- function(df_column,old_value,new_value){
  for(x in 1:length(df_column)){
    if(df_column[x] == old_value)
      df_column[x] = new_value
  }
  
  return(df_column)
}

#Función para obtener el catálogo de jugadores con su id y equipo para cada temporada
get_players_catalog <- function(ruta_file,season){
  #Creamos la ruta final para leer el catálogo de equipos
  ruta_catalogo_equipos <- paste0(ruta_file,'master_team_list.csv')
  #Obtenemos el catálogo de los equipos de la PL
  teams_catalog <- read.csv(ruta_catalogo_equipos)
  #Aplicamos formato a la temporada
  season_format <- gsub('/','',season)
  #Creamos la ruta final para leer el catálogo de jugadores
  ruta_catalogo_jugadores <- paste0(ruta_file,'players_raw_',season_format,'.csv')
  #Obtenemos el catálogo de los jugadores de la PL
  players_catalog <- read.csv(ruta_catalogo_jugadores)
  
  #Del catálogo de jugadores, nos quedamos solamente con las columnas 'id' y 'team'
  players_catalog <- players_catalog %>% select(.,c('id','team'))
  #A la temporada le aplicamos formato para la columna "season"
  long_season <- gsub('/','-',season)
  long_season <- paste0('20',long_season)
  #Al catálogo de jugadores le agregamos la columna de temporada
  players_catalog['season'] <- long_season
  
  #Unimos ambas tablas de catálogos
  ply_tms_catalog <- players_catalog %>% left_join(teams_catalog, by = c('season','team'))
  
  return(ply_tms_catalog)
}