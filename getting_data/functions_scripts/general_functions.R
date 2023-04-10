#Funciones que se utilizan para más de un proceso de creación de tabla de datos
#Autor: Pedro Velázquez

#Instalamos las librerias a utilizar
library(tidyr)

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

#Función para obtener la dirección del repo
get_fpl_repo_directory <- function(){
  ruta_repo <- '/Users/pedrovela/Docs/Fantasy-Premier-League'
  
  return(ruta_repo)
}

#Función para obtener las credenciales de GitHub
my_git_credentials <- function(){
  username <- 'pedro-vr'
  pw <- 'Gtfr45%al07.'
    
  return_list = list("username" = username, "pw" = pw)
  
  return(return_list)
}

#Función para obtener la ruta en donde guardamos la data raw de FPL
get_fpl_directory <- function(){
  ruta_fpl <- '/Users/pedrovela/Docs/Fantasy-Premier-League/data/'
  
  return(ruta_fpl)
}

#Función para obtener el nombre de los archivos para hacer el pull
get_pull_files_names <- function(){
  files_names <- c('cleaned_merged_seasons.csv',
                   '2016-17/gws/merged_gw.csv','2016-17/cleaned_players.csv','2016-17/players_raw.csv',
                   '2017-18/gws/merged_gw.csv','2017-18/cleaned_players.csv','2017-18/players_raw.csv',
                   '2018-19/gws/merged_gw.csv','2018-19/cleaned_players.csv','2018-19/players_raw.csv',
                   '2019-20/gws/merged_gw.csv','2019-20/cleaned_players.csv','2019-20/players_raw.csv',
                   '2020-21/gws/merged_gw.csv','2020-21/cleaned_players.csv','2020-21/players_raw.csv',
                   '2021-22/gws/merged_gw.csv','2021-22/cleaned_players.csv','2021-22/players_raw.csv')
  
  return(files_names)
}

#Función para obtener el nombre con el que se guardarán los archivos derivados del pull
get_pull_destination_files_names <- function(){
  files_names <- c('cleaned_merged_seasons.csv',
                   'merged_gw_1617.csv','cleaned_players_1617.csv','players_raw_1617.csv',
                   'merged_gw_1718.csv','cleaned_players_1718.csv','players_raw_1718.csv',
                   'merged_gw_1819.csv','cleaned_players_1819.csv','players_raw_1819.csv',
                   'merged_gw_1920.csv','cleaned_players_1920.csv','players_raw_1920.csv',
                   'merged_gw_2021.csv','cleaned_players_2021.csv','players_raw_2021.csv',
                   'merged_gw_2122.csv','cleaned_players_2122.csv','players_raw_2122.csv')
  
  return(files_names)
}

#Función para obtener la ruta en donde guardaremos las tablas finales
get_tables_directory <- function(is_work_computer=F){
  if(is_work_computer)
    ruta <- '/Users/pedro.velazquez/Library/Mobile Documents/com~apple~CloudDocs/Documents/Git_repos/var_epl/tables/'
  else
    ruta <- '/Users/pedrovela/Documents/Git_repos/var_epl/tables/'
  
  return(ruta)
}

#Función para obtener la ruta de la info raw
get_raw_directory <- function(is_work_computer=F){
  if(is_work_computer)
    ruta_raw <- '/Users/pedro.velazquez/Library/Mobile Documents/com~apple~CloudDocs/Documents/Git_repos/var_epl/getting_data/raw_data/'
  else
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

#Función para cargar los CSVs de la FPL a las carpetas de info raw
load_raw_csv <- function(ruta_csv,file_names,ruta_guardado,nombres_guardado){
  #Creamos las variables que nos ayudarán con el loop
  num_files <- length(file_names)
  #Creamos el loop para ir agarrando cada nombre de file
  for (x in 1:num_files) {
    #Armamos la ruta final de donde leeremos el csv
    ruta_leer <- paste0(ruta_csv,file_names[x])
    #Leemos el archivo localmente
    csv_raw <- read.csv(ruta_leer)
    #Leemos el nombre correspondiente con el que se guardará el file 
    nombre_guardado <- nombres_guardado[x]
    #Armamos la ruta final para la carpeta raw
    ruta_raw <- paste0(ruta_guardado,nombre_guardado)
    #Guardamos el archivo dentro de la carpeta raw
    write_csv(csv_raw,ruta_raw)
    #Imprimimos de éxito
    print(paste('Archivo',nombre_guardado,'guardado'))
  }
  return('Archivos guardados')
}

#Función para hacer el pull del repo de Fantasy-Premier-League
pull_fpl_data <- function(ruta_fpl_repo,git_username,git_pw,is_work_computer,do_pull,files_names,ruta_raw,nombres_finales){
  #Primero nos fijamos si estamos trabajando con la compu del trabajo
  if(is_work_computer)
    #Si estamos con la compu del trabajo no hacemos pull
    resultado <- 'Dont pull'
  else{
    #Si estamos con la compu personal ahora nos fijamos si queremos hacer el pull
    if(do_pull){
      #Si queremos hacer el pull entonces llamamos la función
      pull(repo = ruta_fpl_repo,
           credentials = cred_user_pass(username = git_username,
                                        password = git_pw))
      #Hacemos la carga de los archivos a la carpeta seleccionada
      load_raw_csv(ruta_fpl,files_names,ruta_raw,nombres_finales)
      #Imprimimos el resultado de que si hicimos el pull
      resultado <- 'Data pulled'
    } else
      resultado <- 'Dont pull'
  }
  
  return(resultado)
}