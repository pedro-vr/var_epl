#Funciones para la lectura y transformación de datos acerca de diferentes estadísticas a nivel jugador dentro de la PL
#Autor: Pedro Velázquez
#Fuente de datos: FBREF

#Función para leer los datos acerca de las estadísticas
get_players_standard_stats <- function(ruta_file){
  #Declaramos la ruta final del archivo
  ruta_ply_std_stats <- paste0(ruta_file,'players_standard_stats.csv')
  
  #Leemos el archivo
  df_ply_std_stats <- read.csv(ruta_ply_std_stats)
  
  #Obtenemos el array con los nombres de las columnas del df
  columnas <- df_ply_std_stats %>% names()
  #Pasamos a minúscula todos los nombres
  columnas <- lapply(columnas,tolower)
  #Los convertimos de nuevo a una lista sencilla para poder asignarlo al df
  columnas <- unlist(columnas)
  
  #Asignamos estos nuevos nombres como columnas del df
  colnames(df_ply_std_stats) <- columnas
  
  #Cambiamos el nombre de la columna
  df_ply_std_stats <- df_ply_std_stats %>% rename(mp_90 = x90s,
                                                  goals_scored = gls,
                                                  assists = ast,
                                                  goals_assists = g.a,
                                                  goals_pen_kicks = g.pk,
                                                  pen_kicks = pk,
                                                  pen_kicks_att = pkatt,
                                                  yellow_cards = crdy,
                                                  red_cards = crdr,
                                                  match_ply = mp,
                                                  npxg_xag = npxg.xag,
                                                  prog_carries = prgc,
                                                  prog_passes = prgp,
                                                  prog_rec = prgr,
                                                  goals_90 = gls_90,
                                                  assists_90 = ast_90,
                                                  goals_assists_90 = g.a_90,
                                                  goals_pen_kicks_90 = g.pk_90,
                                                  goals_assists_pen_kicks_90 = g.a.pk_90,
                                                  xg_xag_90 = xg.xag_90,
                                                  npxg_xag_90 = npxg.xag_90,
                                                  team_name = squad)
  
  return(df_ply_std_stats)
}

#Función para modificar la columna nation ya que contiene más de una clave
nation_column_formatted <- function(df_players){
  
  #Obtenemos la longitud de la columna nation
  num_registros <- dim(df_players)[1]
  #Auxiliar para contar el num de registros
  reg_mod <- 0
  #Vector para guardar las nacionalidades ya con el formato deseado
  nacionalidades_array <- NULL
  
  #Generamos el primer loop en donde ubicamos el espacio " "
  for (registro in 1:num_registros) {
    #Obtenemos cada nacionalidad
    nacionalidad <- df_players$nation[registro]
    #Ubicamos la posición del espacio
    pos_esp <- unlist(gregexpr(' ',nacionalidad))
    #Obtenemos solo la parte de la nacionalidad que nos sirve
    nacionalidad_formatted <- substr(nacionalidad,pos_esp+1,nchar(nacionalidad))
    #Asignamos el nuevo valor al renglón correspondiente
    nacionalidades_array <- c(nacionalidades_array,nacionalidad_formatted) 
    #Aumentamos la cuenta de registros
    reg_mod <- reg_mod + 1
  }
  
  #Asignamos el vector de nacionalidades con formato a la columna deseada
  df_players$nation = nacionalidades_array
  
  #Regresamos el num de registros modificados
  print(paste('Se modificaron',as.character(reg_mod),'registros'))
}