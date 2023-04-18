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

#Función para modificar los registros de nación, para que queden con el formato deseado
nationality_formatted <- function(nation_char){

  #Del char nacion, ubicamos la posición del espacio
  pos_esp <- unlist(gregexpr(' ',nation_char))
  #Obtenemos solo la parte de la nacionalidad que nos sirve
  nacionalidad_formatted <- substr(nation_char,pos_esp+1,nchar(nation_char))
  
  #Regresamos el registro de nación ya modificado
  return(nacionalidad_formatted)
}

#Función para modificar los registros de posición, cuando aplique
position_formatted <- function(position_char){
  
  #Del char posición, obtenemos el número de chars que lo componen
  n_chars <- nchar(position_char)
  #Ahora, si el num de chars > 2 entonces modificamos el registro
  if(n_chars > 2)
    #Si tiene mas de dos characters, nos quedamos solo con los primeros dos
    posicion_formatted <- substr(position_char,1,2)
  else 
    #Si solo tiene dos chars entonces lo dejamos igual
    posicion_formatted <- position_char
  
  #Regresamos el valor de la posición ya formateada
  return(posicion_formatted)
}

#Función para leer la información acerca de porteros en la PL
get_players_gk_stats <- function(ruta_file){
  #Declaramos la ruta final del archivo
  ruta_ply_gk_stats <- paste0(ruta_file,'players_goalkeeping.csv')
  
  #Leemos el archivo
  df_ply_gk_stats <- read.csv(ruta_ply_gk_stats)
  
  #Obtenemos el array con los nombres de las columnas del df
  columnas <- df_ply_gk_stats %>% names()
  #Pasamos a minúscula todos los nombres
  columnas <- lapply(columnas,tolower)
  #Los convertimos de nuevo a una lista sencilla para poder asignarlo al df
  columnas <- unlist(columnas)
  
  #Asignamos estos nuevos nombres como columnas del df
  colnames(df_ply_gk_stats) <- columnas
  
  #Cambiamos el nombre de la columna
  df_ply_gk_stats <- df_ply_gk_stats %>% rename(mp_90 = x90s,
                                                match_ply = mp,
                                                team_name = squad,
                                                gk_goals_against = ga,
                                                gk_goals_against_90 = ga90,
                                                gk_shots_target_against = sota,
                                                gk_saves_perc = save.,
                                                gk_saves = saves,
                                                gk_wins = w,
                                                gk_deuces = d,
                                                gk_losses = l,
                                                gk_clean_sheets = cs,
                                                gk_clean_sheets_perc = cs.,
                                                gk_pen_kicks_att = pkatt,
                                                gk_pen_kicks_allowed = pka,
                                                gk_pen_kicks_saved = pksv,
                                                gk_pen_kicks_missed = pkm,
                                                gk_pen_kicks_saved_perc = save._pk)
  
  return(df_ply_gk_stats)
}