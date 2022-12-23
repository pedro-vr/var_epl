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
