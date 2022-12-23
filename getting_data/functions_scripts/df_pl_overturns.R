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
get_overturns_details <- function(html_data){
  ovrt_details <- getNodeSet(html_data, "//p") %>% sapply(., xmlValue)
  
  return(ovrt_details)
}

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

# -------- Funciones para el df de descripciones de overturns del VAR ------------ 

teams_ovrt_details <- function(array_data){
  #Creamos los vectores en donde vamos a guardar todos los valores numéricos de los overturns 
  #para todos los equipos en las dos temporadas
  des_ovrt <- NULL
  indices_overturns <- which(array_data == "Overturns:")
  num_indices <- length(indices_overturns)
  
  #Iniciamos los loops en donde vamos a recorrer todos los datos de la página web y solo nos quedamos
  #con los valores numéricos, todo esto para las dos temporadas. Se hacen en dos loops diferentes 
  #porque el tamaño del vector es diferente entre ambas temporadas
  
  for(m in 1:num_indices){
    if(m == num_indices){
      #a partir del overturn buscamos el indice del siguiente "Game:"
      inicio_seq <- indices_overturns[m]
      fin_seq <- length(array_data)
      indices_games <- which(array_data[inicio_seq:fin_seq] == 'Game:')
      lim_seq <- indices_games[1] - 1
      aux_cont <- 0
      for(n in seq(inicio_seq+1,inicio_seq+lim_seq,by=2)){
        metrica_ovrt <- array_data[n]
        if(grepl("/",metrica_ovrt)){
          indice_caracter <- unlist(gregexpr("/",metrica_ovrt))
          metrica1 <- as.integer(substr(metrica_ovrt,1,indice_caracter-1))
          metrica2 <- as.integer(substr(metrica_ovrt,indice_caracter+1,nchar(metrica_ovrt)))
          net_metrica <- metrica1 - metrica2
          des_ovrt <- c(des_ovrt,net_metrica)
          aux_cont <- aux_cont + 1
        } else {
          des_ovrt <- c(des_ovrt,as.integer(metrica_ovrt))
          aux_cont <- aux_cont + 1
        }
      }
      if(aux_cont < 11){
        des_ovrt <- c(des_ovrt,rep(2019,11-aux_cont))
      }
    } else {
      #a partir del overturn buscamos el indice del siguiente "Game:"
      inicio_seq <- indices_overturns[m]
      fin_seq <- indices_overturns[m+1]
      indices_games <- which(array_data[inicio_seq:fin_seq] == 'Game:')
      lim_seq <- indices_games[1] - 1
      aux_cont <- 0
      for(n in seq(inicio_seq+1,inicio_seq+lim_seq,by=2)){
        metrica_ovrt <- array_data[n]
        if(grepl("/",metrica_ovrt)){
          indice_caracter <- unlist(gregexpr("/",metrica_ovrt))
          metrica1 <- as.integer(substr(metrica_ovrt,1,indice_caracter-1))
          metrica2 <- as.integer(substr(metrica_ovrt,indice_caracter+1,nchar(metrica_ovrt)))
          net_metrica <- metrica1 - metrica2
          des_ovrt <- c(des_ovrt,net_metrica)
          aux_cont <- aux_cont + 1
        } else {
          des_ovrt <- c(des_ovrt,as.integer(metrica_ovrt))
          aux_cont <- aux_cont + 1
        }
      }
      if(aux_cont < 11){
        des_ovrt <- c(des_ovrt,rep(2019,11-aux_cont))
      }
    }
  }
  #20_21 : net_penalties (for - against)
  #21_22 : net_penalties (for - against) , net_red_cards (for - against)
  return(des_ovrt)
}

#Función para meter los valores de cada métrica en un dataframe 
teams_ovrt_details_df <- function(data_array,teams_names,season){
  #Overturns totales del equipo
  overturns_array <- NULL
  #leading goals for
  lgf_array <- NULL
  #disallowed goals for  
  dgf_array <- NULL
  #leading goals against
  lga_array <- NULL
  #disallowed goals against
  dga_array <- NULL
  #net goal score
  ngs_array <- NULL
  #subjective decisions for
  sdf_array <- NULL
  #subjective decisions against
  sda_array <- NULL
  #net subjective score
  nsc_array <- NULL
  #penalties net score (for - against)
  pns_array <- NULL
  #red cards net score (for - against)
  rcns_array <- NULL
  
  #ya que tenemos los vectores creados entonces recorremos el vector con todos los valores numéricos
  #y vamos guardando cada dato en su respectivo vector. Esto aplica para ambas temporadas
  
  tamano_array <- length(data_array)
  num_metricas <- 11
  
  for (x in seq(1,tamano_array,by = num_metricas)) {
    overturns_array <- c(overturns_array,as.integer(data_array[x]))
    lgf_array <- c(lgf_array,as.integer(data_array[x+1]))
    dgf_array <- c(dgf_array,as.integer(data_array[x+2]))
    lga_array <- c(lga_array,as.integer(data_array[x+3]))
    dga_array <- c(dga_array,as.integer(data_array[x+4]))
    ngs_array <- c(ngs_array,as.integer(data_array[x+5]))
    sdf_array <- c(sdf_array,as.integer(data_array[x+6]))
    sda_array <- c(sda_array,as.integer(data_array[x+7]))
    nsc_array <- c(nsc_array,as.integer(data_array[x+8]))
    pns_array <- c(pns_array,as.integer(data_array[x+9]))
    rcns_array <- c(rcns_array,as.integer(data_array[x+10]))
  }
  
  num_equipos <- tamano_array / num_metricas
  season_array <- c(rep(season,num_equipos))
  #Ya que tenemos los vectores individuales de cada métrica entonces creamos la tabla de los 
  #equipos con estas nuevas métricas 
  des_df <- data.frame("season" = season_array,
                       "team" = teams_names,
                       "ovrt" = overturns_array,"lgf" = lgf_array,"dgf" = dgf_array,
                       "lga" = lga_array,"dga" = dga_array,"ngs" = ngs_array,
                       "sdf" = sdf_array,"sda" = sda_array,"nsc" = nsc_array,
                       'pns' = pns_array,"rcns" = rcns_array)
  
  return(des_df)
}

#Función para solo obtener la lista de equipos de los overturns
get_ovrt_teams <- function(ruta_raw,season,is_21_22 = F){
  #Leemos la data html de la temporada indicada
  ovrt_data <- read_html_data(ruta_raw,season)
  #Obtenemos la tabla de overturns de la temporada indicada
  ovrt_table <- get_overturns_table(ovrt_data)
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
  
  return(ovrt_teams_array)
}

#Función para obtener el df de las descripciones de intervenciones del VAR
get_pl_overturns_desc <- function(ruta_raw,season,is_21_22 = F){
  #Leemos la data html de la temporada indicada
  ovrt_data <- read_html_data(ruta_raw,season)
  #Obtenemos el detalle de las decisiones del VAR para cada equipo dentro de la temporada indicada
  ovrt_details <- get_overturns_details(ovrt_data)
  #Obtenemos la data limpia de la tabla de las descripciones de overturns
  ovrt_desc_clean <- clean_ovrt_data(ovrt_details)
  #En los datos de la temporada 21/22 hay un error en un dato que hace tener varios NA
  #separaremos los datos para corregir el error
  if(is_21_22){
    details_data_p1 <- ovrt_desc_clean[1:614]
    aux_nombre1 <- substr(ovrt_desc_clean[615],1,unlist(gregexpr(":",ovrt_desc_clean[615])))
    aux_num1 <- substr(ovrt_desc_clean[615],unlist(gregexpr(":",ovrt_desc_clean[615]))+2,nchar(ovrt_desc_clean[615]))
    details_data_p2 <- ovrt_desc_clean[616:777]
    aux_nombre2 <- substr(ovrt_desc_clean[778],1,unlist(gregexpr(":",ovrt_desc_clean[778])))
    aux_num2 <- substr(ovrt_desc_clean[778],unlist(gregexpr(":",ovrt_desc_clean[778]))+2,nchar(ovrt_desc_clean[778]))
    details_data_p3 <- ovrt_desc_clean[779:length(ovrt_desc_clean)]
    ovrt_desc_clean <- c(details_data_p1,aux_nombre1,aux_num1,details_data_p2,aux_nombre2,aux_num2,details_data_p3)
  }
  #Obtenemos las métricas relacionadas a los overturns de los equipos 
  #Si la temporada es 20/21 eliminamos el registro de más que tiene
  if(season == '20/21'){
    ovrt_desc <- teams_ovrt_details(ovrt_desc_clean)
    ovrt_desc <- ovrt_desc[-133]
  } else
    ovrt_desc <- teams_ovrt_details(ovrt_desc_clean)
  #Obtenemos la lista con el nombre de los equipos de la temporada indicada
  ovrt_teams_array <- get_ovrt_teams(ruta_raw,season,is_21_22)
  #Obtenemos el df final con las métricas individuales
  ovrt_desc_df <- teams_ovrt_details_df(ovrt_desc,ovrt_teams_array[1:20],season)
  
  return(ovrt_desc_df)
}

# -------- Funciones para el df de descripciones de intervenciones del VAR ------------ 

#funcion para dividir los detalles de incidentes del VAR por cada equipo
var_desc_arrays <- function(data_array,data_teams_names){
  #Vector en donde guardaremos el detalle del equipo al que se enfrentó, localia y fecha del incidente
  game_detail <- NULL
  #Vector en donde guardaremos la descripción del incidente y el tipo de este (a favor o en contra)
  incident_detail <- NULL
  #Vector auxiliar que nos va a ayudar a delimitar los incidentes de cada equipo
  aux_index <- NULL
  
  #Loop que nos sirve para guardar el índice de comienzo de cada equipo, 
  #la descripción de la localía y fecha y la descripción de los incidentes 
  #por equipo de la temporada correspondiente
  for(s in 1:length(data_array)){
    if(data_array[s] == "Overturns:")
      #Guardamos el índice en donde inicia las descripciones de cada equipo
      aux_index <- c(aux_index,s)
    else if(data_array[s] == "Game:")
      #Guardamos el detalle del equipo contrario, fecha y localía
      game_detail <- c(game_detail,data_array[s+1])
    else if(data_array[s] == "Incident:")
      #Guardamos la descripción del incidente y el tipo de éste
      incident_detail <- c(incident_detail,data_array[s+1])
  }
  
  #agregamos el tamaño del vector de la data al vector auxiliar para referencia
  aux_index <- c(aux_index,length(data_array))
  
  #Vector en donde guardaremos la cuenta del total de incidentes por equipo
  cuenta_equipo <- NULL
  
  #Loop en donde guardamos el número total de incidentes por equipo para la temporada correspondiente
  for(t in 1:(length(aux_index)-1)){
    #Contador auxiliar que contará el número de incidentes por equipo
    cont <- 0
    for(u in seq(aux_index[t],aux_index[t+1])){
      #verificamos si el dato que nos encontramos es un incidente
      if(data_array[u] == "Incident:")
        #Si si es un incidente entonces aumentamos la cuenta en el auxiliar
        cont <- cont + 1
    }
    #Al final guardamos la cuenta total de incidentes por equipo en el vector nulo 
    cuenta_equipo <- c(cuenta_equipo,cont)
  }
  
  #Vectores que guardaran los nombres de los equipos repetidos
  teams_names_rep <- NULL
  
  #Iniciamos el loop en donde ya repetimos el nombre de los equipos
  for(v in 1:length(cuenta_equipo)){
    teams_names_rep <- c(teams_names_rep,rep(data_teams_names[v],cuenta_equipo[v]))
  }
  
  lista_vectores <- list("aux_index" = aux_index,"game_detail" = game_detail,
                         "incident_detail" = incident_detail,"teams_names_rep" = teams_names_rep)
  
  return(lista_vectores)
}

#Función de donde obtendremos la descripción, minuto y tipo de incidente del VAR
var_desc <- function(data_array){
  #Vector en donde guardaremos la descripción como tal del incidente
  desc_incident <- NULL
  #Vector en donde guardaremos el minuto en que ocurrió el incidente
  min_incident <- NULL
  #Vector en donde guardaremos el tipo de incidente
  type_incident <- NULL
  
  for(y in 1:length(data_array)){
    #Por cada descripción de incidente, obtenemos el o los números de índice en donde 
    #se encuentra alguna comna, esto porque lo que separa la descripción del incidente y el 
    #minuto en donde ocurrio es una coma
    coma_1 <- str_locate_all(data_array[y],",")
    #Por cada descripción de incidente, obtenemos el o los números de índice en donde 
    #se encuentra algún guión, esto porque lo que separa el minuto del incidente y el 
    #tipo del mismo es un guión
    guion_1 <- str_locate_all(data_array[y],"-")
    
    #en caso de que si haya comas, nos traemos la posición de la última coma
    coma_2 <- coma_1[[1]][dim(coma_1[[1]])[1]]
    #en caso de que si haya guion, nos traemos la posición del último guión
    guion_2 <- guion_1[[1]][dim(guion_1[[1]])[1]]
    
    #numero de comas que tiene la descripcion
    num_comas <- dim(coma_1[[1]])
    #numero de guiones que tiene la descripcion
    num_guiones <- dim(guion_1[[1]])
    
    #nos fijamos si la descripcion no tiene comas
    if(num_comas[1] == 0){
      #si no tiene comas quiere decir que viene solamente la pura descripción en texto
      desc_incident <- c(desc_incident,data_array[y])
      #no contiene el minuto
      min_incident <- c(min_incident,0)
      #no contiene el tipo de incidente
      type_incident <- c(type_incident,"None")
      #ahora nos fijamos si contiene el minuto
    } else if (!grepl("minute",data_array[y])){
      #Traemos la pura descripción del incidente
      desc_incident <- c(desc_incident,substr(data_array[y],1,guion_2-2))
      #Traemos el puro tipo de incidente
      type_incident <- c(type_incident,substr(data_array[y],guion_2+2,str_length(data_array[y])))
      #En este caso no hay minuto de incidente entonces le asignamos un cero
      min_incident <- c(min_incident,0)
      #en este caso es donde si tiene descripción, minuto y tipo
    } else {
      #Traemos la pura descripción del incidente
      desc_incident <- c(desc_incident,substr(data_array[y],1,coma_2-1))
      #Traemos el puro tipo de incidente
      type_incident <- c(type_incident,substr(data_array[y],guion_2+2,str_length(data_array[y])))
      #Traemos el puro minuto del incidente
      min_texto <- substr(data_array[y],coma_2+2,guion_2-2)
      #obtenemos la posición del espacio
      espacio <- str_locate(min_texto, " ")[1]
      #nos quedamos con el puro número
      min_entero <- as.integer(substr(min_texto,1,espacio-3))
      #guardamos el minuto en el array
      min_incident <- c(min_incident,min_entero)
    }
  }
  
  lista_descripciones <- list("description" = desc_incident, "minute" = min_incident, 
                              "type" = type_incident)
  
  return(lista_descripciones)
}

#Función para obtener el detalle de los equipos de los incidentes del VAR
var_desc_teams <- function(data_array,aux,data_game_detail){
  #Vector en donde guardaremos los nombres de los equipos contrincantes
  juegos <- NULL
  #Vector en donde guardaremos si es que el equipo es local o visitante
  localia <- NULL
  #Vector en donde guardaremos la fecha del partido en donde suscitó el incidente
  fecha <- NULL
  #Variable que nos ayudará a controlar el número de incidentes que se suscitaron en un 
  #mismo partido
  suma <- 0
  
  #Loop en donde separaremos el nombre del equipo contrincante, el tipo de localia y la fecha del incidente
  for(z in 1:(length(aux)-1)){
    #Vector que nos va a ayudar a concatenar los índices en donde se encuentra un partido para cada equipo
    vector1 <- NULL
    #Loop en donde obtenemos el índice donde se encuentra cada partido para cada equipo
    for(w in seq(aux[z],aux[z+1])){
      #verificamos que sea un partido
      if(data_array[w] == 'Game:'){
        #Si sí es un partido entonces guardamos el número de índice
        vector1 <- c(vector1,w)
      }
    }
    #Al final, cuando ya tengamos todos los índices, concatenamos el último para abarcar todos los juegos
    vector1 <- c(vector1,aux[z+1])
    #Vector nulo que nos ayudará a saber cuántos incidentes hubo dentro de cada juego para cada equipo
    cont_total <- NULL
    #Loop para guardar el número de incidentes para cada partido de cada equipo
    for(t in 1:(length(vector1)-1)){
      #Variable que va a acumular el número de incidentes por partido
      cont_inc <- 0
      #Loop que nos va a ayudar a identificar los incidentes por partido y sumarlos
      for(u in seq(vector1[t],vector1[t+1])){
        #Verificamos que sea un incidente
        if(data_array[u] == 'Incident:'){
          #Si sí es un incidente entonces incrementamos la cuenta
          cont_inc <- cont_inc + 1
        }
      }
      #Al final concatenamos el total de incidentes por cada partido
      cont_total <- c(cont_total,cont_inc)
    }
    #Ya que tenemos el conteo de incidentes por cada partido de cada equipo, hacemos el loop para 
    #entonces si separar todos los datos del partido (equipo contrincante, localia y fecha)
    for(v in 1:length(cont_total)){
      #Encontramos el índice en donde se encuentra el primer paréntesis
      parentesis_1 <- str_locate(data_game_detail[suma+v],"\\(")[1]
      #Encontramos el índice en donde se encuentra el segundo paréntesis
      parentesis_2 <- str_locate(data_game_detail[suma+v],"\\)")[1]
      #Encontramos el índice en donde se encuentra el punto y coma
      punto_coma <- str_locate(data_game_detail[suma+v],";")[1]
      #Nos traemos el nombre del equipo contrincante y lo repetimos por el número de 
      #incidentes que tuvo en un mismo partido
      juegos <- c(juegos,rep(substr(data_game_detail[suma+v],1,parentesis_1-2),cont_total[v]))
      #Nos traemos la localia y lo repetimos por el número de incidentes que tuvo en un mismo partido
      localia <- c(localia,rep(substr(data_game_detail[suma+v],parentesis_1+1,punto_coma-1),
                               cont_total[v]))
      #Nos traemos la fecha del partido y lo repetimos por el número de incidentes que tuvo en un 
      #mismo partido
      fecha <- c(fecha,rep(substr(data_game_detail[suma+v],punto_coma+2,parentesis_2-1),
                           cont_total[v]))
    }
    #Aumentamos la variable para controlar los datos a repetir
    suma <- suma + length(cont_total)
  }
  
  lista_equipos <- list("teams" = juegos,"venue_type" = localia, "date" = fecha)
  
  return(lista_equipos)
}


#Función para obtener el df con los detalles de las intervenciones del VAR en la PL
get_pl_var_details <- function(ruta_raw,season,is_21_22=F){
  #Obtenemos la data html de la temporada indicada
  ovrt_data <- read_html_data(ruta_raw,season)
  #Obtenemos la tabla de overturns de la temporada indicada
  ovrt_table <- get_overturns_table(ovrt_data)
  #Obtenemos la data limpia de la tabla de overturns
  ovrt_clean <- clean_ovrt_data(ovrt_table)
  #Obtenemos la lista con el nombre de los equipos de la temporada indicada
  ovrt_teams_array <- ovrt_teams_names_points(ovrt_clean,is_21_22)$names
  #Obtenemos el detalle de las decisiones del VAR para cada equipo dentro de la temporada indicada
  var_details <- get_overturns_details(ovrt_data)
  #Obtenemos la data limpia de la tabla de las descripciones de overturns
  var_details_clean <- clean_ovrt_data(var_details)
  #Obtenemos el detalle de los juegos e incidentes por cada equipo de las dos temporadas
  #Vector auxiliar que nos va a ayudar a delimitar los incidentes de cada equipo
  aux_array <- var_desc_arrays(var_details_clean,ovrt_teams_array)$aux_index
  #Vector en donde guardaremos la descripción del incidente y el tipo de este (a favor o en contra)
  incident_detail <- var_desc_arrays(var_details_clean,ovrt_teams_array)$incident_detail
  #Vector en donde guardaremos el detalle del equipo al que se enfrentó, localia y fecha del incidente
  game_detail <- var_desc_arrays(var_details_clean,ovrt_teams_array)$game_detail
  #Vector que guardará los nombres de los equipos repetidos por cada incidente que haya tenido
  #en cada temporada
  teams_names_rep <- var_desc_arrays(var_details_clean,ovrt_teams_array)$teams_names_rep
  
  #Ya que tenemos el nombre de los equipos repetidos entonces empezamos por desglozar la descripción
  #de los incidentes que tuvo cada equipo, se dividen en la descripción como tal, el minuto en que
  #ocurrió el incidente y el tipo de incidente (a favor o en contra)
  
  #Vector en donde guardaremos la descripción como tal del incidente
  desc_incident <- var_desc(incident_detail)$description
  #Vector en donde guardaremos el minuto en que ocurrió el incidente
  min_incident <- var_desc(incident_detail)$minute
  #Vector en donde guardaremos el tipo de incidente
  type_incident <- var_desc(incident_detail)$type
  
  #Ya que tenemos los minutos en número entonces iniciamos el proceso para desglozar los datos 
  #de cada partido en donde ocurrió un incidente por equipo
  
  #Vector en donde guardaremos los nombres de los equipos contrincantes
  juegos <- var_desc_teams(var_details_clean,aux_array,game_detail)$teams
  #Vector en donde guardaremos si es que el equipo es local o visitante
  localia <- var_desc_teams(var_details_clean,aux_array,game_detail)$venue_type
  #Vector en donde guardaremos la fecha del partido en donde suscitó el incidente
  fechas <- var_desc_teams(var_details_clean,aux_array,game_detail)$date
  
  #Ya que tenemos todos los datos entonces armamos la tabla final con todas estas descripciones
  df_var_details <- data.frame("season" = c(rep(season,length(incident_detail))),
                               "team1" = teams_names_rep,
                               "team2" = juegos, 
                               "home/away" = localia, 
                               "date" = fechas,
                               "incident" = desc_incident,
                               "type" = type_incident,
                               "min" = min_incident)
  
  return(df_var_details)
}

# -------- Funciones para el df de descripciones de intervenciones del VAR sin columna de localia------------

#Función para que, a partir del df de las descripciones de var, desglozar los campos team1 y team2 
#en donde ya se considera al team1 como el local y al team2 como el visitante
var_desc_1 <- function(data_frame){
  #Vectores que nos ayudarán a guardar los equipos locales y visitantes
  team1 <- NULL
  team2 <- NULL
  
  for(a in 1:dim(data_frame)[1]){
    #Transformamos el primer equipo en character
    equipo1 <- as.character(data_frame$team1[a])
    #Transformamos el segundo equipo en character
    equipo2 <- as.character(data_frame$team2[a])
    #Transformamos el indicador de localia en character
    local_eq <- as.character(data_frame$home.away[a])
    #Verificamos si el indicador de localia dice que el primer equipo es local
    if(local_eq == 'H'){
      #Si sí es local entonces agregamos el primer equipo al vector de los locales
      team1 <- c(team1,equipo1)
      #Si sí es local entonces agregamos el segundo equipo al vector de los visitantes
      team2 <- c(team2,equipo2)
    } else {
      #Si no es local entonces agregamos el segundo equipo al vector de los locales
      team1 <- c(team1,equipo2)
      #Si no es local entonces agregamos el primer equipo al vector de los visitantes
      team2 <- c(team2,equipo1)
    }
  }
  
  lista_equipos <- list("local_team" = team1, "away_team" = team2)
  
  return(lista_equipos)
}

#Función para obtener la tabla final de descripciones de incidentes del VAR sin columna de localia
get_pl_var_details_2 <- function(ruta_raw,season,is_21_22=F){
  #Obtenemos la tabla de var_details que servirá de base aquí
  var_details <- get_pl_var_details(ruta_raw,season,is_21_22)
  #Vector que contiene a equipos locales
  team1 <- var_desc_1(var_details)$local_team
  #Vector que contiene a equipos visitantes
  team2 <- var_desc_1(var_details)$away_team
  
  #De la tabla var_details quitamos las columnas "home.away", "team2" y "team1"
  var_details_aux <- var_details %>% select(-c('home.away','team2','team1'))
  #Creamos la tabla final con las nuevas columnas 'team1' y 'team2'
  var_details1 <- var_details_aux %>% bind_cols(team1,team2)
  #Renombramos las columnas de los equipos
  #Obtenemos el vector con los nombres de las columnas
  col_names <- names(var_details1)
  #Obtenemos los nombres de las ultimas dos columnas recién agregadas
  col_name1 <- col_names[length(col_names) - 1]
  col_name2 <- col_names[length(col_names)]
  #Hacemos la reasignación de nombres
  var_details1 <- var_details1 %>% rename("team1" = col_name1, "team2" = col_name2)
  
  return(var_details1)
}