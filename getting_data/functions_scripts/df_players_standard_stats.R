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

#Función para leer la información acerca de tiros en la PL
get_players_shooting_stats <- function(ruta_file){
  #Declaramos la ruta final del archivo
  ruta_ply_shooting_stats <- paste0(ruta_file,'players_shooting.csv')
  
  #Leemos el archivo
  df_ply_shooting_stats <- read.csv(ruta_ply_shooting_stats)
  
  #Obtenemos el array con los nombres de las columnas del df
  columnas <- df_ply_shooting_stats %>% names()
  #Pasamos a minúscula todos los nombres
  columnas <- lapply(columnas,tolower)
  #Los convertimos de nuevo a una lista sencilla para poder asignarlo al df
  columnas <- unlist(columnas)
  
  #Asignamos estos nuevos nombres como columnas del df
  colnames(df_ply_shooting_stats) <- columnas
  
  #Cambiamos el nombre de la columna
  df_ply_shooting_stats <- df_ply_shooting_stats %>% rename(mp_90 = x90s,
                                                            team_name = squad,
                                                            sh_goals_scored = gls,
                                                            sh_shots = sh,
                                                            sh_shots_target = sot,
                                                            sh_shots_target_perc = sot.,
                                                            sh_shots_90 = sh.90,
                                                            sh_shots_target_90 = sot.90,
                                                            sh_goals_per_shot = g.sh,
                                                            sh_goals_per_shot_target = g.sot,
                                                            sh_avg_shot_distance = dist,
                                                            sh_shots_free_kicks = fk,
                                                            sh_pen_kicks = pk,
                                                            sh_pen_kicks_att = pkatt)
  
  return(df_ply_shooting_stats)
}

#Función para leer la información acerca de pases en la PL
get_players_passing_stats <- function(ruta_file){
  #Declaramos la ruta final del archivo
  ruta_ply_passing_stats <- paste0(ruta_file,'players_passing.csv')
  
  #Leemos el archivo
  df_ply_passing_stats <- read.csv(ruta_ply_passing_stats)
  
  #Obtenemos el array con los nombres de las columnas del df
  columnas <- df_ply_passing_stats %>% names()
  #Pasamos a minúscula todos los nombres
  columnas <- lapply(columnas,tolower)
  #Los convertimos de nuevo a una lista sencilla para poder asignarlo al df
  columnas <- unlist(columnas)
  
  #Asignamos estos nuevos nombres como columnas del df
  colnames(df_ply_passing_stats) <- columnas
  
  #Cambiamos el nombre de la columna
  df_ply_passing_stats <- df_ply_passing_stats %>% rename(mp_90 = x90s,
                                                          team_name = squad,
                                                          passes_completed = cmp,
                                                          passes_attempted = att,
                                                          pass_comp_perc = cmp.,
                                                          total_pass_dist = totdist,
                                                          prog_pass_dist = prgdist,
                                                          short_passes_completed = cmp.1,
                                                          short_passes_attempted = att.1,
                                                          short_pass_comp_perc = cmp..1,
                                                          medium_passes_completed = cmp.2,
                                                          medium_passes_attempted = att.2,
                                                          medium_pass_comp_perc = cmp..2,
                                                          long_passes_completed = cmp.3,
                                                          long_passes_attempted = att.3,
                                                          long_pass_comp_perc = cmp..3,
                                                          assists = ast,
                                                          assists_xag = a.xag,
                                                          key_passes = kp,
                                                          passes_final_third = x01.mar,
                                                          passes_pen_area = ppa,
                                                          crosses_pen_area = crspa,
                                                          prog_passes = prgp)
  
  return(df_ply_passing_stats)
}

#Función para leer la información acerca del tipo de pases en la PL
get_players_passing_types_stats <- function(ruta_file){
  #Declaramos la ruta final del archivo
  ruta_ply_passing_types_stats <- paste0(ruta_file,'players_passing_types.csv')
  
  #Leemos el archivo
  df_ply_passing_types_stats <- read.csv(ruta_ply_passing_types_stats)
  
  #Obtenemos el array con los nombres de las columnas del df
  columnas <- df_ply_passing_types_stats %>% names()
  #Pasamos a minúscula todos los nombres
  columnas <- lapply(columnas,tolower)
  #Los convertimos de nuevo a una lista sencilla para poder asignarlo al df
  columnas <- unlist(columnas)
  
  #Asignamos estos nuevos nombres como columnas del df
  colnames(df_ply_passing_types_stats) <- columnas
  
  #Cambiamos el nombre de la columna
  df_ply_passing_types_stats <- df_ply_passing_types_stats %>% rename(mp_90 = x90s,
                                                                      team_name = squad,
                                                                      passes_attempted = att,
                                                                      live_ball_passes = live,
                                                                      dead_ball_passes = dead,
                                                                      free_kicks_passes = fk,
                                                                      through_ball_passes = tb,
                                                                      switches = sw,
                                                                      crosses = crs,
                                                                      throw_ins = ti,
                                                                      corner_kicks = ck,
                                                                      inswinging_corner_kicks = `in`,
                                                                      outswinging_corner_kicks = out,
                                                                      straight_corner_kicks = str,
                                                                      passes_completed = cmp,
                                                                      passes_offside = off,
                                                                      blocked_passes = blocks)
  
  return(df_ply_passing_types_stats)
}

#Función para leer la información acerca de creación de tiros y goles en la PL
get_players_goals_shoots_creation_stats <- function(ruta_file){
  #Declaramos la ruta final del archivo
  ruta_ply_goals_shoots_creation_stats <- paste0(ruta_file,'players_goal_shot_creation.csv')
  
  #Leemos el archivo
  df_ply_goals_shoots_creation_stats <- read.csv(ruta_ply_goals_shoots_creation_stats)
  
  #Obtenemos el array con los nombres de las columnas del df
  columnas <- df_ply_goals_shoots_creation_stats %>% names()
  #Pasamos a minúscula todos los nombres
  columnas <- lapply(columnas,tolower)
  #Los convertimos de nuevo a una lista sencilla para poder asignarlo al df
  columnas <- unlist(columnas)
  
  #Asignamos estos nuevos nombres como columnas del df
  colnames(df_ply_goals_shoots_creation_stats) <- columnas
  
  #Cambiamos el nombre de la columna
  df_ply_goals_shoots_creation_stats <- df_ply_goals_shoots_creation_stats %>% rename(mp_90 = x90s,
                                                                                      team_name = squad,
                                                                                      shot_creating_actions = sca,
                                                                                      shot_creating_actions_90 = sca90,
                                                                                      sca_live_passes = passlive,
                                                                                      sca_dead_passes = passdead,
                                                                                      sca_take_ons = to,
                                                                                      sca_shots = sh,
                                                                                      sca_fouls_drawn = fld,
                                                                                      sca_def = def,
                                                                                      goal_creating_actions = gca,
                                                                                      goal_creating_actions_90 = gca90,
                                                                                      gca_live_passes = passlive.1,
                                                                                      gca_dead_passes = passdead.1,
                                                                                      gca_take_ons = to.1,
                                                                                      gca_shots = sh.1,
                                                                                      gca_fouls_drawn = fld.1,
                                                                                      gca_def = def.1)
  
  return(df_ply_goals_shoots_creation_stats)
}

#Función para leer la información acerca de acciones defensivas en la PL
get_players_defensive_actions_stats <- function(ruta_file){
  #Declaramos la ruta final del archivo
  ruta_ply_defensive_actions_stats <- paste0(ruta_file,'players_defensive_actions.csv')
  
  #Leemos el archivo
  df_ply_defensive_actions_stats <- read.csv(ruta_ply_defensive_actions_stats)
  
  #Obtenemos el array con los nombres de las columnas del df
  columnas <- df_ply_defensive_actions_stats %>% names()
  #Pasamos a minúscula todos los nombres
  columnas <- lapply(columnas,tolower)
  #Los convertimos de nuevo a una lista sencilla para poder asignarlo al df
  columnas <- unlist(columnas)
  
  #Asignamos estos nuevos nombres como columnas del df
  colnames(df_ply_defensive_actions_stats) <- columnas
  
  #Cambiamos el nombre de la columna
  df_ply_defensive_actions_stats <- df_ply_defensive_actions_stats %>% rename(mp_90 = x90s,
                                                                              team_name = squad,
                                                                              tackles = tkl,
                                                                              tackles_won = tklw,
                                                                              tackles_def_third = def.3rd,
                                                                              tackles_mid_third = mid.3rd,
                                                                              tackles_att_third = att.3rd,
                                                                              dribblers_tackled = tkl.1,
                                                                              dribbles_challenged = att,
                                                                              dribblers_tackled_perc = tkl.,
                                                                              challenges_lost = lost,
                                                                              shots_blocked = sh,
                                                                              passes_blocked = pass,
                                                                              interceptions = int,
                                                                              tackles_interceptions = tkl.int,
                                                                              clearances = clr,
                                                                              errors = err)
  
  return(df_ply_defensive_actions_stats)
}

#Función para leer la información acerca de la posesión de balón en la PL
get_players_possession_stats <- function(ruta_file){
  #Declaramos la ruta final del archivo
  ruta_ply_possession_stats <- paste0(ruta_file,'players_possession_stats.csv')
  
  #Leemos el archivo
  df_ply_possession_stats <- read.csv(ruta_ply_possession_stats)
  
  #Obtenemos el array con los nombres de las columnas del df
  columnas <- df_ply_possession_stats %>% names()
  #Pasamos a minúscula todos los nombres
  columnas <- lapply(columnas,tolower)
  #Los convertimos de nuevo a una lista sencilla para poder asignarlo al df
  columnas <- unlist(columnas)
  
  #Asignamos estos nuevos nombres como columnas del df
  colnames(df_ply_possession_stats) <- columnas
  
  #Cambiamos el nombre de la columna
  df_ply_possession_stats <- df_ply_possession_stats %>% rename(mp_90 = x90s,
                                                                team_name = squad,
                                                                touches_def_pen = def.pen,
                                                                touches_def_third = def.3rd,
                                                                touches_mid_third = mid.3rd,
                                                                touches_att_third = att.3rd,
                                                                touches_att_pen = att.pen,
                                                                live_touches = live,
                                                                take_ons_att = att,
                                                                take_ons_succ = succ,
                                                                take_ons_succ_perc = succ.,
                                                                tackled = tkld,
                                                                tackled_perc = tkld.,
                                                                carrying_dist = totdist,
                                                                prog_carrying_dist = prgdist,
                                                                prog_carries = prgc,
                                                                carries_final_third = x01.mar,
                                                                carries_pen = cpa,
                                                                miscontrols = mis,
                                                                dispossessed = dis,
                                                                passes_received = rec,
                                                                prog_passes_received = prgr)
  
  return(df_ply_possession_stats)
}