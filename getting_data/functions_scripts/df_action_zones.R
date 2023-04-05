#Funciones para la lectura y transformación de datos acerca de las zonas de acción en los partidos de la PL
#Autor: Pedro Velázquez
#Fuente de datos: WhoScored.com

#Función para leer los datos acerca de las estadísticas
get_action_zones <- function(ruta_file){
  #Declaramos la ruta final del archivo
  ruta_action_zones <- paste0(ruta_file,'pl_action_zones.csv')
  
  #Leemos el archivo
  df_action_zones <- read.csv(ruta_action_zones)
  
  #Cambiamos el nombre de la columna
  df_action_zones <- df_action_zones %>% rename(season = Season,
                                                team_name = Team,
                                                own_third = Own.Third,
                                                middle_third = Middle.Third,
                                                opposition_third = Opposition.Third)
  
  return(df_action_zones)
}