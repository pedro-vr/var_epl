#Funciones para la lectura y transformación de datos acerca de estadísticas de partidos de la PL
#Autor: Pedro Velázquez
#Fuente de datos: FBREF

#Función para leer los datos acerca de las estadísticas
get_poss_stats <- function(ruta_file){
  #Declaramos la ruta final del archivo
  ruta_poss_stats <- paste0(ruta_file,'epl_stats_poss.csv')
  
  #Leemos el archivo
  df_poss_stats <- read.csv(ruta_poss_stats)
  
  #Cambiamos el nombre de la columna
  df_poss_stats <- df_poss_stats %>% rename(min_played = miin_played)
  
  return(df_poss_stats)
}