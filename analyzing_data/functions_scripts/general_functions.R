#Funciones que se utilizan para más de un proceso de análisis de los datos
#Autor: Pedro Velázquez

#Función para obtener la ruta del archivo del proceso ETL de la data
get_etl_file_directory <- function(is_work_computer=F){
  if(is_work_computer)
    ruta_etl <- '/Users/pedro.velazquez/Library/Mobile Documents/com~apple~CloudDocs/Documents/Git_repos/var_epl/getting_data/'
  else
    ruta_etl <- '/Users/pedrovela/Documents/Git_repos/var_epl/getting_data/'
  
  return(ruta_etl)
}

#Función que hace que dispare o no, el documento Rmd de ETL
render_etl_file <- function(will_render = F,ruta_render,is_work_computer=F){
  if(will_render){
    xfun::Rscript_call(
      rmarkdown::render,
      list(input = ruta_render, 
           output_format = 'pdf_document', 
           params = list(is_work_computer=is_work_computer))
    )
    resultado <- 'It rendered'
  } else {
    resultado <- 'Dont render'
  }
  
  return(resultado)
}

#Función para obtener la ruta de las imágenes a incluir en el Rmd
get_images_directory <- function(is_work_computer=F){
  if(is_work_computer)
    ruta_imagen <- '/Users/pedro.velazquez/Library/Mobile Documents/com~apple~CloudDocs/Documents/Git_repos/var_epl/images/'
  else
    ruta_imagen <- '/Users/pedrovela/Documents/Git_repos/var_epl/images/'
  
  return(ruta_imagen)
}

#Funcion para leer la tabla a analizar
get_df <- function(df_number,is_work_computer=F){
  
  ruta_personal = '/Users/pedrovela/Documents/Git_repos/var_epl/tables/'
  ruta_trabajo = '/Users/pedro.velazquez/Library/Mobile Documents/com~apple~CloudDocs/Documents/Git_repos/var_epl/tables/' 
  
  df_name <- switch (as.character(df_number),
                     '1' = 'premier_league.csv', 
                     '2' = 'pl_overturns_var.csv', 
                     '3' = 'var_details.csv',
                     '4' = 'var_details1.csv', 
                     '5' = 'stats_epl.csv', 
                     '6' = 'fpl_merged_seasons.csv',
                     '7' = 'fpl_merged_players.csv',
                     '8' = 'fpl_merged_gw.csv',
                     '9' = 'epl_poss_stats.csv',
                     '10' = 'pl_action_zones.csv',
                     '11' = 'pl_players_general_stats.csv'
  )
  
  if(is_work_computer)
    df_path <-paste(ruta_trabajo,df_name,sep = "")
  else 
    df_path <- paste(ruta_personal,df_name,sep = "")
  
  df_final <- read.csv(df_path)
}

#Función para sustituir el valor de un registro de una columna por otro
sub_value_df <- function(df_column,old_value,new_value){
  for(x in 1:length(df_column)){
    if(df_column[x] == old_value)
      df_column[x] = new_value
  }
  
  return(df_column)
}