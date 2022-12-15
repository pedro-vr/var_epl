#Funciones para la lectura y transformación de datos de la PL obtenida de la página web de ESPN
#Autor: Pedro Velázquez
#Fuente de datos: ESPN

#Instalamos las librerias a utilizar
library(tidyr)
library(XML)
library(xml2)

#Función para leer la data de tipo html de la temporada indicada
read_html_data <- function(ruta,season){
  season_formato <- gsub(season,pattern = "/",replacement = "-")
  nombre_file <- paste('VAR_in_PL_',season_formato,'.html',sep = "")
  ruta_final <- paste(ruta,nombre_file,sep = "")
  
  data_final <- htmlParse(ruta_final)
  
  return(data_final)
}

#Función para obtener la tabla de overturns de la temporada indicada, la cual sabemos que 
#están en los nodos de la forma <aside class = "inline editorial float-r">
get_overturns_table <- function(html_data){
  ovrt_table <- getNodeSet(html_data, "//aside[@class='inline editorial float-r']") %>% sapply(., xmlValue)
  
  return(ovrt_table)
}

#Función para obtener el nombre de los equipos junto a sus puntos a favor o en contra a raíz de 
#decisiones del VAR (netscore), los cuales sabemos que están en nodos de la forma <h2> dentro de 
#la info de overturns, 
#get_overturns_teams <- function(html_data){
#  ovrt_teams <- getNodeSet(html_data, "//h2") %>% sapply(., xmlValue)
#  
#  return(ovrt_teams)
#}

#Función para obtener el detalle de cada decisión hecha por el VAR hacia cada equipo de la PL
#dentro de la info de overturns, el cual sabemos que está en nodos del tipo <p>
#get_overturns_details <- function(html_data){
#  ovrt_details <- getNodeSet(html_data, "//p") %>% sapply(., xmlValue)
#  
#  return(ovrt_details)
#}

#Función para limpiar los datos de los equipos dentro de la info de overturns
clean_ovrt_data <- function(ovrt_data){
  #Se va a guardar todos los datos "servibles" o "útiles" dentro de un solo array
  ovrt_clean_array <- NULL
  #Vamos a hacer un loop para guardar los datos acerca de los datos de overturns de la 
  #temporada indicada
  for (a in 1:length(ovrt_data)) {
    #Guardamos cada jornada como una matriz
    ovrt_clean_array <- c(ovrt_clean_array,str_split(ovrt_data[a],"\n",simplify = T))
  }
  
  #Ya que tenemos los datos de todos los equipos de la temporada indicada, 
  #ahora nos vamos a quedar solamente con los datos útiles, para esto, hacemos un loop, 
  #vamos a trimear cada entrada y si la longitud de la entrada después de trimearla 
  #es mayor a cero entonces es un dato útil y lo guardamos en un nuevo array
  
  #Array en donde vamos a guardar todo
  ovrt_data_final <- NULL
  #Iniciamos el loop en donde vamos a guardar todos los datos
  for (b in 1:length(ovrt_clean_array)) {
    #Trimeamos los datos para ver is es que hay datos útiles
    data_trim <- str_trim(ovrt_clean_array[b])
    #Si la longitud de lo trimeado es mayor a cero entonces tiene datos útiles
    if(str_length(data_trim) > 0){
      #Guardamos el dato útil en un array 
      ovrt_data_final <- c(ovrt_data_final,data_trim)
    }
  }
  
  return(ovrt_data_final)
}

#Función para corregir registros de algunos equipos de la temporads 21/22 (solo aplica a esa temporada)
fix_ovrt_teams <- function(ovrt_data){
  #Dividimos la data para solo tratar aquellos registros que estén erróneos
  ovrt_data_aux1 <- ovrt_data[1:72]
  ovrt_data_aux2 <- ovrt_data[75:length(ovrt_data)]
  #Arreglamos el primer registro
  spaces_aux1 <- unlist(gregexpr(' ',ovrt_data[73]))
  last_space_aux1 <- spaces_aux1[length(spaces_aux1)]
  team_aux1 <- substr(ovrt_data[73],1,last_space_aux1-1)
  points_aux1 <- substr(ovrt_data[73],last_space_aux1+1,nchar(ovrt_data[73]))
  #Arreglamos el segundo registro
  spaces_aux2 <- unlist(gregexpr(' ',ovrt_data[74]))
  last_space_aux2 <- spaces_aux2[length(spaces_aux2)]
  team_aux2 <- substr(ovrt_data[74],1,last_space_aux2-1)
  points_aux2 <- substr(ovrt_data[74],last_space_aux2+1,nchar(ovrt_data[74]))
  #Volvemos a unir la data con los registros ya corregidos
  ovrt_data_fixed <- c(ovrt_data_aux1,team_aux1,points_aux1,team_aux2,points_aux2,ovrt_data_aux2)
  
  return(ovrt_data_fixed)
}

#Función para obtener los arrays con los nombres de los equipos y sus puntajes de overturns
ovrt_teams_names_points <- function(ovrt_data,is_21_22 = F){
  if(is_21_22){
    #nombres importantes del tipo de decisión de VAR
    var_names <- c('VAR overturns - net score','VAR overturns - decisions against','VAR overturns - decisions for')
  }else{
    #nombres importantes del tipo de decisión de VAR
    var_names <- c('VAR overturns (net score)','VAR overturns - decisions for','VAR overturns - decisions against')
  }
  #Vector para guardar la posición de cada nombre de decisión de VAR dentro del array de datos
  var_names_pos <- NULL
  #Vector para guardar el nombre de cada equipo
  ovrt_team_names <- NULL
  #Vector para guardar los puntos asociados a cada equipo por tipo de decisión de VAR
  ovrt_team_points <- NULL 
  
  #Loop para obtener los números de índice de los nombres del VAR dentro del array de datos
  for (x in 1:length(var_names)) {
    name_pos <- which(ovrt_data == var_names[x])
    var_names_pos <- c(var_names_pos,name_pos)
  }
  
  for (y in 1:length(var_names_pos)) {
    if(y == 3){
      for(w in seq(var_names_pos[y] + 1,length(ovrt_data),by=2)){
        ovrt_team_names <- c(ovrt_team_names,ovrt_data[w])
        ovrt_team_points <- c(ovrt_team_points,as.integer(ovrt_data[w+1]))
      }
    } else {
      for(w in seq(var_names_pos[y] + 1,var_names_pos[y+1] - 1,by=2)){
        ovrt_team_names <- c(ovrt_team_names,ovrt_data[w])
        ovrt_team_points <- c(ovrt_team_points,as.integer(ovrt_data[w+1]))
      }
    }
  }
  
  return_list = list("names" = ovrt_team_names, "points" = ovrt_team_points)
  
  return(return_list)
}

#Función para obtener la tabla final de pl_overturns
get_ovrt_df <- function(ovrt_points,ovrt_teams,ovrt_season){
  #array con la temporada
  ovrt_season_final <- c(rep(ovrt_season,20))
  #array con los equipos de net score
  teams_ns <- c(ovrt_teams[1:20])
  #array con los equipos de decisions for
  teams_df <- c(ovrt_teams[21:40])
  #array con los equipos de decisions against
  teams_da <- c(ovrt_teams[41:60])
  #array con los puntos net score
  points_ns <- c(ovrt_points[1:20])
  #array con los puntos decisions for
  points_df <- c(ovrt_points[21:40])
  #array con los puntos decisions against
  points_da <- c(ovrt_points[41:60])
  
  #armamos el df de cada métrica por separado
  #df de net score
  ovrt_ns <- data.frame("season" = ovrt_season_final, "team" = teams_ns, 
                        "ns" = points_ns)
  #df de decisions for
  ovrt_df <- data.frame("season" = ovrt_season_final, "team" = teams_df, 
                        "df" = points_df)
  #df de decisions against
  ovrt_da <- data.frame("season" = ovrt_season_final, "team" = teams_da, 
                        "da" = points_da)
  
  #armamos el df final
  ovrt_df_final <- inner_join(ovrt_ns,ovrt_df,by=c("season","team")) %>% 
    inner_join(.,ovrt_da,by=c("season","team"))
  
  return(ovrt_df_final)
}

#Función para ejecutar todas las funciones anteriores en uno solo
get_pl_overturns <- function(ruta_raw,season,is_21_22 = F){
  #Leemos la data html de la temporada indicada
  ovrt_data <- read_html_data(ruta_raw,season)
  #Obtenemos la tabla de overturns de la temporada indicada
  ovrt_table <- get_overturns_table(ovrt_data)
  #Obtenemos los equipos de overturns de la temporada indicada
  #ovrt_teams_19_20 <- get_overturns_teams(ovrt_19_20)
  #Obtenemos el detalle de las decisiones del VAR para cada equipo dentro de la temporada indicada
  #ovrt_details_19_20 <- get_overturns_details(ovrt_19_20)
  #Obtenemos la data limpia de la tabla de overturns
  ovrt_clean <- clean_ovrt_data(ovrt_table)
  #Nos fijamos si la temporada es 19/20, si lo es entonces acortamos el array
  if(season == '19/20')
    ovrt_clean <- ovrt_clean[1:123]
  #Nos fijamos si la temporada es 21/22, si lo es entonces aplicamos función para 
  #arreglar registros de nombres de equipos
  else if(is_21_22)
    ovrt_clean <- fix_ovrt_teams(ovrt_clean)
  else
    ovrt_clean <- ovrt_clean
  #Obtenemos la lista con el nombre de los equipos de la temporada indicada
  ovrt_teams_array <- ovrt_teams_names_points(ovrt_clean,is_21_22)$names
  #Obtenemos la lista con los puntos asociados a cada equipo de la temporada indicada
  ovrt_points <- ovrt_teams_names_points(ovrt_clean,is_21_22)$points
  #Obtenemos el df final de overturns para la temporada indicada
  ovrt_df <- get_ovrt_df(ovrt_points,ovrt_teams_array,season)
  
  return(ovrt_df)
}