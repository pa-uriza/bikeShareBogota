# Datos necesarios proyecto PG2

#Cargo las librerias necesarias

library(tidyverse)
library(sf)
library(NbClust)
library(factoextra)
library(osrm)
library(reshape)
library(DescTools)
library(dplyr)
library(geosphere)
library(readxl)
library(lubridate)
library(XML)
library(RCurl)
library(gmapsdistance)

#Defino la ruta de los archivos con los cuales voy a trabajar

RutaBaseDatos1<-"/Users/lukasbogota/Desktop/PG2/Datos Lukas PG2/"
RutaBaseDatos2<-"/Users/lukasbogota/Desktop/"
RutaBaseDatos3<-"/Users/lukasbogota/Desktop/Analisis de Datos/PG2 Lukas/USC/Modos/"

#Se determinan las franjas horarias a tener en cuenta
InicioF1 <- hm("05:00")
InicioF2 <- hm("06:00")
InicioF3 <- hm("07:00")
InicioF4 <- hm("08:00")
InicioF5 <- hm("09:00")
InicioF6 <- hm("10:00")
InicioF7 <- hm("11:00")
InicioF8 <- hm("12:00")
InicioF9 <- hm("13:00")
InicioF10 <- hm("14:00")
InicioF11 <- hm("15:00")
InicioF12 <- hm("16:00")
InicioF13 <- hm("17:00")
InicioF14 <- hm("18:00")
InicioF15 <- hm("19:00")
InicioF16 <- hm("20:00")
InicioF17 <- hm("21:00")
FinF17 <- hm("22:00")

# Se carga la informaci??n
  load("~/Desktop/PG2/Datos Lukas PG2/viajes.Rdata")
  Encuesta_Movilidad <- read("~/Desktop/Analisis de Datos/Encuesta Movilidad.xlsx")
  Encuesta_Movilidad<-transform(Encuesta_Movilidad,id=interaction(ID_ENCUESTA,NUMERO_PERSONA,NUMERO_VIAJE,sep='_'))
  Encuesta_Movilidad<- merge(Encuesta_Movilidad, viajes, all = TRUE, by = c('id'))
  Encuesta_Movilidad<-Encuesta_Movilidad %>% filter(distance!="NA") 

  ViajesMotorizados<- Encuesta_Movilidad %>% transmute(id,ponderador_calibrado_viajes,hora_inicio,hora_fin,distance,longitud_origen,latitud_origen,longitud_destino,latitud_destino,MEDIO_PREDOMINANTE)
  ViajesMotorizados<-ViajesMotorizados%>% filter(MEDIO_PREDOMINANTE!="OTROS")%>% filter(MEDIO_PREDOMINANTE!="ILEGAL") %>% filter(MEDIO_PREDOMINANTE!="ESPECIAL") 
  
  #Se cambia el Id de los viajes por el #de columna
  
  ViajesMotorizados <- ViajesMotorizados %>% rename(c(id="ID_Viaje")) %>% mutate(ID_Viaje=row_number())

  #Filtro los datos para solo tener en cuenta viajes de menos de 13 km de longitud, m??s de 1 km de longitud y asigno franja horaria

  ViajesMotorizados <- ViajesMotorizados %>% filter(distance < 13e3)%>% filter(distance > 1000)  %>%
    mutate(T = case_when((hora_inicio > InicioF1) & (hora_inicio < InicioF2) ~ 1,
                       hora_inicio > InicioF2 & hora_inicio < InicioF3 ~ 2,
                       hora_inicio > InicioF3 & hora_inicio < InicioF4 ~ 3,
                       hora_inicio > InicioF4 & hora_inicio < InicioF5 ~ 4,
                       hora_inicio > InicioF5 & hora_inicio < InicioF6 ~ 5,
                       hora_inicio > InicioF6 & hora_inicio < InicioF7 ~ 6,
                       hora_inicio > InicioF7 & hora_inicio < InicioF8 ~ 7,
                       hora_inicio > InicioF8 & hora_inicio < InicioF9 ~ 8,
                       hora_inicio > InicioF9 & hora_inicio < InicioF10 ~ 9,
                       hora_inicio > InicioF10 & hora_inicio < InicioF11 ~ 10,
                       hora_inicio > InicioF11 & hora_inicio < InicioF12 ~ 11,
                       hora_inicio > InicioF12 & hora_inicio < InicioF13 ~ 12,
                       hora_inicio > InicioF13 & hora_inicio < InicioF14 ~ 13,
                       hora_inicio > InicioF14 & hora_inicio < InicioF15 ~ 14,
                       hora_inicio > InicioF15 & hora_inicio < InicioF16 ~ 15,
                       hora_inicio > InicioF16 & hora_inicio < InicioF17 ~ 16,
                       hora_inicio > InicioF17 & hora_inicio < FinF17 ~ 17,
                       TRUE ~ 0)) %>% filter(T != 0)


  #Cargo la informaci??n de la ubicaci??n de los parqueaderos p??blicos en Bogot??
  load(paste0(RutaBaseDatos1, "parqueaderos.Rdata"))
  parqueaderos<-rename(parqueaderos,c("ID_Parquedero"="ID_Parqueadero"))


  #Cargo el layer de las localidades y los transformo a las coordenadas deseadas. 

  LayerLocalidad<-st_read(paste0(RutaBaseDatos2,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "Loca",stringsAsFactors = FALSE) %>% st_transform(4326)

  
#Se filtran los parqueaderos que se encuentran en las localidades deseadas

  #Identifico la localidad de Usaquen y Chapinero y transformo las coordenadas
  LayerLocalidad <- LayerLocalidad %>% filter(LocNombre %in% c("CHAPINERO","USAQUEN","SUBA")) %>% st_transform(3116)
  
  
  #Filtro los parqueaderos por localidad, borrando aquellos que no pertenencen y asigno como ID el n??mero de columna
  
  parqueaderos <- parqueaderos %>% st_join(select(LayerLocalidad, LocNombre)) %>% filter(!is.na(LocNombre)) %>% transmute(ID_Parqueadero=row_number())


  
#Se establecen como posibles estaciones a aquellos parqueaderos que tengan un nodo de origen o un nodo de destino a menos de 500 mts
  
  #Se crea una capa con los origenes, y se tranforma a las coordenadas deseadas
  viajesOrigen <- ViajesMotorizados %>% st_as_sf(coords=c(6,7))  %>% transmute(ID_Viaje)  %>% mutate(ID_O= ID_Viaje) %>%  st_set_crs(4326) %>% st_transform(3116)
  
  #Se crea una capa con los destinos, y se tranforma a las coordenadas deseadas
  viajesDestino<- ViajesMotorizados %>% st_as_sf(coords=c(8,9)) %>% transmute(ID_Viaje) %>% mutate(ID_D= ID_Viaje) %>%  st_set_crs(4326)%>% st_transform(3116)
  
  #Se identifica si el parqueadero sirve para cubrir alg??n viaje
  Estaciones <- parqueaderos %>% st_join(select(st_buffer(viajesOrigen, dist=500),ID_O),largest = TRUE) 
  Estaciones <- Estaciones %>% st_join(select(st_buffer(viajesDestino, dist=500),ID_D),largest = TRUE) 
  Estaciones <- filter(Estaciones,(!is.na(ID_O) && !is.na(ID_D)))
  
  #Se obtiene el conjunto de estaciones resultantes
  Estaciones <- Estaciones %>% transmute(NE=ID_Parqueadero)
  Estaciones$NE<-sub("^", "E", Estaciones$NE)
  
  
#Se verifica si un viaje (par origen-destino) puede ser realizado
  
  # se identifica y se filtran los viajes que desde el origen tienen una estaci??n a menos de 500mts
  viajesOrigen<- viajesOrigen %>% st_join(st_buffer(Estaciones, dist=500),largest = TRUE) %>% filter(!(is.na(NE))) 
  
  # se identifica y se filtran los viajes que hacia el destino tienen una estaci??n a menos de 500mts
  viajesDestino<- viajesDestino %>% st_join(st_buffer(Estaciones, dist=500),largest = TRUE) %>% filter(!(is.na(NE))) 
  
  # Se filtran los viajes que cumplan ambas condiciones
  ViajesPosibles<-ViajesMotorizados%>% left_join(st_set_geometry(viajesOrigen,NULL)) %>% filter(!(is.na(NE))) %>% subset(select = -c(NE) )
  ViajesPosibles<-ViajesPosibles%>% left_join(st_set_geometry(viajesDestino,NULL)) %>% filter(!(is.na(NE))) %>% subset(select = -c(NE) )
  ViajesPosibles <- ViajesPosibles %>% mutate(ID_Viaje=row_number())
  
  rm(viajes,viajesDestino,viajesOrigen,parqueaderos,LayerLocalidad,ViajesMotorizados,Encuesta_Movilidad)  
  
  
#Se identifican los nodos de origen
  
  NodosOrigen <- ViajesPosibles %>% st_as_sf(coords=c(6,7)) %>% transmute(ID_Viaje) %>%  st_set_crs(4326) %>% st_transform(3116)
  NodosOrigen<-NodosOrigen%>% mutate(NO= sub("^", "O", NodosOrigen$ID_Viaje ))
  
  #Se identifican los nodos de destino
  
  NodosDestino <- ViajesPosibles %>% st_as_sf(coords=c(8,9)) %>% transmute(ID_Viaje) %>%  st_set_crs(4326) %>% st_transform(3116)
  NodosDestino<-NodosDestino%>% mutate(ND=sub("^", "D", NodosDestino$ID_Viaje))
  
  
#Se identifican los posibles arcos del sistema
  
  # Arcos desde el nodo de origen hacia la estaci??n de pr??stamo 
  
  Arcos_NO_NE <- NodosOrigen %>% st_join(st_buffer(Estaciones, dist=500))  %>% rename(c(NE="NE_O")) 
  rownames(Arcos_NO_NE) <- NULL
  
  #Arcos desde la estaci??n de devoluci??n hasta el nodo de destino
  
  Arcos_NE_ND <- NodosDestino %>% st_join(st_buffer(Estaciones, dist=500))  %>% rename(c(NE="NE_D"))
  rownames(Arcos_NE_ND) <- NULL
  
  #Arcos entre las estaciones de pr??stamo y de entrega (para cada posible viaje)
  Arcos_NE_NE <- Arcos_NO_NE %>% st_set_geometry(NULL) %>% left_join(Arcos_NE_ND %>% st_set_geometry(NULL))
  
  
  #Acomodo los arcos al formato del modelo de Pyomo
  
  Arcos_NE_NE <- Arcos_NE_NE %>% transmute(NO,ND,NI=NE_O,NJ=NE_D)
  Arcos_NE_NE <- Arcos_NE_NE %>% filter(NI!=NJ)
  
  Arcos_NO_NE <- Arcos_NO_NE %>% transmute(NO=sub("^", "O", Arcos_NO_NE$ID_Viaje),ND=sub("^", "D", Arcos_NO_NE$ID_Viaje),NI=NO,NJ=NE_O)
  Arcos_NO_NE<-Arcos_NO_NE%>% st_set_geometry(NULL)
  
  Arcos_NE_ND <- Arcos_NE_ND %>% transmute(NO=sub("^", "O", Arcos_NE_ND$ID_Viaje),ND=sub("^", "D", Arcos_NE_ND$ID_Viaje),NI=NE_D,NJ=ND)
  Arcos_NE_ND<-Arcos_NE_ND%>% st_set_geometry(NULL)
  
  Arcos_Sink<-ViajesPosibles %>% transmute (NO=sub("^", "O", ViajesPosibles$ID_Viaje),ND=sub("^", "D", ViajesPosibles$ID_Viaje),NI=sub("^", "O", ViajesPosibles$ID_Viaje),NJ=sub("^", "D", ViajesPosibles$ID_Viaje))
  
  
  Arcos <- rbind(Arcos_NE_NE,Arcos_NO_NE,Arcos_NE_ND,Arcos_Sink)
  
  
 #Se identifica la demanda del sistema
  
  DemandaOrigen<- ViajesPosibles %>% transmute (NO=sub("^", "O", ViajesPosibles$ID_Viaje),ND=sub("^", "D", ViajesPosibles$ID_Viaje),NI=sub("^", "O", ViajesPosibles$ID_Viaje),T,b = ViajesPosibles$ponderador_calibrado_viajes)
  DemandaDestino<- ViajesPosibles %>% transmute (NO=sub("^", "O", ViajesPosibles$ID_Viaje),ND=sub("^", "D", ViajesPosibles$ID_Viaje),NI=sub("^", "D", ViajesPosibles$ID_Viaje),T,b = -ViajesPosibles$ponderador_calibrado_viajes)
  
  DemandaViajes <- rbind(DemandaOrigen,DemandaDestino)
  rm(DemandaOrigen,DemandaDestino)  
  

 #Asignar costo a cada arco (determinar distancia de cada arco)
  
  
 #Se calcula la distancia entre cada par de estaciones
  
  Estaciones_google<-Estaciones %>%  st_transform(4326) %>% st_coordinates() %>% as.data.frame() %>% transmute(NE=row_number(),lon=X,lat=Y,coord= interaction(lat,lon,sep='+'))
  
 
  Viajes_NE_NE <- Arcos_NE_NE %>% transmute(NI,NJ) %>% group_by(NI,NJ) %>% filter(row_number()==1) %>% ungroup()
  Viajes_NE_NE <- Viajes_NE_NE %>% filter(NI!=NJ)
  Viajes_NE_NE_google <- Viajes_NE_NE %>% mutate(Distancia=0)%>% mutate(Tiempo=0)
  Viajes_NE_NE_google$NI <- as.numeric(gsub("[\\E,]", "", Viajes_NE_NE_google$NI))
  Viajes_NE_NE_google$NJ <- as.numeric(gsub("[\\E,]", "", Viajes_NE_NE_google$NJ))
  
  VPromBicicleta<-4.3
  
  set.api.key("##########")
  

  
  pb <- txtProgressBar(min = 1, max = nrow(Viajes_NE_NE_google), style = 3)
  for (i in 1:nrow(Viajes_NE_NE_google)){
    
    results <- gmapsdistance(origin = Estaciones_google[Viajes_NE_NE_google$NI[i],]$coord,
                              destination = Estaciones_google[Viajes_NE_NE_google$NJ[i],]$coord,
                              mode = "walking",dep_date = "2019-06-12",
                             dep_time = "09:00:00")  
   
     if ( is.null(results$Time)){
       Viajes_NE_NE_google$Distancia[i] <- NA
       Viajes_NE_NE_google$Tiempo[i] <- NA
    }else{
      Viajes_NE_NE_google$Distancia[i] <-results$Distance
      Viajes_NE_NE_google$Tiempo[i] <- results$Distance/VPromBicicleta
    }
    
    Sys.sleep(0.1)
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  save(Viajes_NE_NE_google,file=paste0(RutaBaseDatos3,"Viajes_NE_NE_google,Rdata"))
 
#Se calcula la distancia entre cada nodo de origen y las estaciones que se encuentren a menos de 500 mts
  
  Viajes_NO_NE <- Arcos_NO_NE %>% transmute(NI,NJ)
  Viajes_NO_NE_google <- Viajes_NO_NE %>% mutate(Distancia=0)%>% mutate(Tiempo=0)
  Viajes_NO_NE_google$NI <- as.numeric(gsub("[\\O,]", "", Viajes_NO_NE_google$NI))
  Viajes_NO_NE_google$NJ <- as.numeric(gsub("[\\E,]", "", Viajes_NO_NE_google$NJ))
  
  
  NodosOrigen_google <- ViajesPosibles %>% transmute(ID_Viaje,lon=longitud_origen,lat=latitud_origen,coord= interaction(lat,lon,sep='+'))

  
  
  
  pb <- txtProgressBar(min = 1, max = nrow(Viajes_NO_NE_google), style = 3)
  for (i in 1:nrow(Viajes_NO_NE_google)){
    
    results <- gmapsdistance(origin = NodosOrigen_google[Viajes_NO_NE_google$NI[i],]$coord,
                             destination = Estaciones_google[Viajes_NO_NE_google$NJ[i],]$coord,
                             mode = "walking",dep_date = "2019-06-12",
                             dep_time = "09:00:00") 
    
    if ( is.null(results$Time)){
      Viajes_NO_NE_google$Distancia[i] <- NA
      Viajes_NO_NE_google$Tiempo[i] <- NA
    }else{
      Viajes_NO_NE_google$Distancia[i] <-results$Distance
      Viajes_NO_NE_google$Tiempo[i] <- results$Time
    }
    
    Sys.sleep(0.1)
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  save(Viajes_NO_NE_google,file=paste0(RutaBaseDatos3,"Viajes_NO_NE_google.Rdata"))
  

  
#Se calcula la distancia entre la estaci??n de entrega y cada nodo de destino posibles
  
  Viajes_NE_ND_google <-  Arcos_NE_ND %>% transmute(NI,NJ)%>% mutate(Distancia=0)%>% mutate(Tiempo=0)
  Viajes_NE_ND_google$NI <- as.numeric(gsub("[\\E,]", "", Viajes_NE_ND_google$NI))
  Viajes_NE_ND_google$NJ <- as.numeric(gsub("[\\D,]", "", Viajes_NE_ND_google$NJ))
  
  NodosDestino_google <- ViajesPosibles %>% transmute(ID_Viaje,lon=longitud_destino,lat=latitud_destino,coord= interaction(lat,lon,sep='+'))
  
  pb <- txtProgressBar(min = 1, max = nrow(Viajes_NE_ND_google), style = 3)
  for (i in 1:nrow(Viajes_NE_ND_google)){
    
    results <- gmapsdistance(origin = Estaciones_google[Viajes_NE_ND_google$NI[i],]$coord,
                             destination = NodosDestino_google[Viajes_NE_ND_google$NJ[i],]$coord,
                             mode = "walking",dep_date = "2019-06-12",
                                                dep_time = "09:00:00")    
    
    if ( is.null(results$Time)){
      
      Viajes_NE_ND_google$Distancia[i] <- NA
      Viajes_NE_ND_google$Tiempo[i] <- NA
    }else{
      Viajes_NE_ND_google$Distancia[i] <-results$Distance
      Viajes_NE_ND_google$Tiempo[i] <- results$Time
    }
    
    Sys.sleep(0.1)
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  save(Viajes_NE_ND_google,file=paste0(RutaBaseDatos3,"Viajes_NE_ND_google.Rdata"))
  
  
#Se calcula la distancia entre nodo de origen y de destino mediante medio de transporte
  
  Arcos_Sink<-ViajesPosibles %>% transmute (NO=sub("^", "O", ViajesPosibles$ID_Viaje),ND=sub("^", "D", ViajesPosibles$ID_Viaje),NI=sub("^", "O", ViajesPosibles$ID_Viaje),NJ=sub("^", "D", ViajesPosibles$ID_Viaje),MEDIO_PREDOMINANTE)
  Arcos_Sink<-Arcos_Sink %>% transmute(NI,NJ,MEDIO_PREDOMINANTE) 
  
  
  Viajes_NO_ND_google <-  Arcos_Sink %>% mutate(Distancia=0)%>% mutate(Tiempo=0)
  Viajes_NO_ND_google$NI <- as.numeric(gsub("[\\O,]", "", Viajes_NO_ND_google$NI))
  Viajes_NO_ND_google$NJ <- as.numeric(gsub("[\\D,]", "", Viajes_NO_ND_google$NJ))
  Hora_Inicio_Viaje<- ViajesPosibles%>% transmute(NI=ID_Viaje,hora_inicio)
  Viajes_NO_ND_google<- merge(Viajes_NO_ND_google, Hora_Inicio_Viaje, by = c('NI'))
  
  pb <- txtProgressBar(min = 1, max = nrow(Viajes_NO_ND_google), style = 3)
  for (i in 1:nrow(Viajes_NO_ND_google)){
    
    
    if ( Viajes_NO_ND_google$MEDIO_PREDOMINANTE[i] == "AUTO"){
      
      results <- gmapsdistance(origin = NodosOrigen_google[Viajes_NO_ND_google$NI[i],]$coord,
                                destination = NodosDestino_google[Viajes_NO_ND_google$NJ[i],]$coord,
                                mode = "driving",dep_date = "2019-06-12",
                               dep_time = Viajes_NO_ND_google$hora_inicio[i])   
      
      Viajes_NO_ND_google$Distancia[i] <- results$Distance
      Viajes_NO_ND_google$Tiempo[i] <- results$Time
    } 
    else if ( Viajes_NO_ND_google$MEDIO_PREDOMINANTE[i] == "PEATON"){
      
      results <- gmapsdistance(origin = NodosOrigen_google[Viajes_NO_ND_google$NI[i],]$coord,
                               destination = NodosDestino_google[Viajes_NO_ND_google$NJ[i],]$coord,
                               mode = "walking",dep_date = "2019-06-12",
                               dep_time = Viajes_NO_ND_google$hora_inicio[i])  
      
      Viajes_NO_ND_google$Distancia[i] <- results$Distance
      Viajes_NO_ND_google$Tiempo[i] <- results$Time
    } 
    
    else if ( Viajes_NO_ND_google$MEDIO_PREDOMINANTE[i] == "MOTO"){
      
      results <- gmapsdistance(origin = NodosOrigen_google[Viajes_NO_ND_google$NI[i],]$coord,
                               destination = NodosDestino_google[Viajes_NO_ND_google$NJ[i],]$coord,
                               mode = "driving",dep_date = "2019-06-12",
                               dep_time = Viajes_NO_ND_google$hora_inicio[i])  
      
      Viajes_NO_ND_google$Distancia[i] <- results$Distance
      Viajes_NO_ND_google$Tiempo[i] <- results$Time/1.5
    } 
    else if ( Viajes_NO_ND_google$MEDIO_PREDOMINANTE[i] == "TAXI"){
      
      results <- gmapsdistance(origin = NodosOrigen_google[Viajes_NO_ND_google$NI[i],]$coord,
                               destination = NodosDestino_google[Viajes_NO_ND_google$NJ[i],]$coord,
                               mode = "driving",dep_date = "2019-06-12",
                               dep_time = Viajes_NO_ND_google$hora_inicio[i]) 
      
      Viajes_NO_ND_google$Distancia[i] <- results$Distance
      Viajes_NO_ND_google$Tiempo[i] <- results$Time
    } 
    else{
      
      results <- gmapsdistance(origin = NodosOrigen_google[Viajes_NO_ND_google$NI[i],]$coord,
                               destination = NodosDestino_google[Viajes_NO_ND_google$NJ[i],]$coord,
                               mode = "transit",dep_date = "2019-06-12",
                               dep_time = Viajes_NO_ND_google$hora_inicio[i])  
      
      Viajes_NO_ND_google$Distancia[i] <- results$Distance
      Viajes_NO_ND_google$Tiempo[i] <- results$Time
    }
    
    Sys.sleep(0.1)
    setTxtProgressBar(pb, i)
  }
  
  close(pb)
  
  save(Viajes_NO_ND_google,file=paste0(RutaBaseDatos3,"Viajes_NO_ND_google.Rdata"))
  
  
  #Defino costos de transitar cada tipo de arco por distancia recorrida
  
  Costo_Segundo <- 1.7
 
  Costo_Pasaje <- 2400
  
  Costo_Banderazo <- 1800
  
  Costo_Segundo_Taxi <- 2.3
  
  Costo_Segundo_Carro <- 1.78
  
  Costo_Segundo_Moto<-0.6
  
  # Para los arcos sink se calcula el costo 
  
  VajesSink_costo <- Arcos_Sink %>% mutate(c = 0)
  
  
  
  for (i in 1:nrow(VajesSink_costo)){
    
    
    if ( Viajes_NO_ND_google$MEDIO_PREDOMINANTE[i] == "AUTO"){
      
      VajesSink_costo$c[i] <- Viajes_NO_ND_google$Tiempo[i]* Costo_Segundo + Viajes_NO_ND_google$Tiempo[i]*Costo_Segundo_Carro
      
    } 
    else if ( Viajes_NO_ND_google$MEDIO_PREDOMINANTE[i] == "PEATON"){
      
      VajesSink_costo$c[i] <- Viajes_NO_ND_google$Tiempo[i]*Costo_Segundo
    } 
    
    else if ( Viajes_NO_ND_google$MEDIO_PREDOMINANTE[i] == "MOTO"){
      
      VajesSink_costo$c[i] <- Viajes_NO_ND_google$Tiempo[i]* Costo_Segundo + Viajes_NO_ND_google$Tiempo[i]*Costo_Segundo_Moto
      
    } 
    else if ( Viajes_NO_ND_google$MEDIO_PREDOMINANTE[i] == "TAXI"){
      
      VajesSink_costo$c[i] <- Viajes_NO_ND_google$Tiempo[i]* Costo_Segundo + Costo_Banderazo + Viajes_NO_ND_google$Tiempo[i]* Costo_Segundo_Taxi
    } 
    else{
      
        VajesSink_costo$c[i] <- Viajes_NO_ND_google$Tiempo[i]* Costo_Segundo + Costo_Pasaje
    }
    
  }
  
  
  VajesSink_costo<- VajesSink_costo %>% transmute(NI,NJ,c)
  
  
  # se calcula el costo para los arcos NO NE
  
  Viajes_NO_NE_costo <- Viajes_NO_NE_google  %>% transmute(NI,NJ,c=(Viajes_NO_NE_google$Tiempo * Costo_Segundo ))
  Viajes_NO_NE_costo$NI <-sub("^", "O", Viajes_NO_NE_costo$NI)
  Viajes_NO_NE_costo$NJ <-sub("^", "E", Viajes_NO_NE_costo$NJ)
  
  # se calcula el costo para los arcos NE NE
  
  Viajes_NE_NE_costo <- Viajes_NE_NE_google  %>% transmute(NI,NJ,c=(Viajes_NE_NE_google$Tiempo * Costo_Segundo ))
  Viajes_NE_NE_costo$NI <-sub("^", "E", Viajes_NE_NE_costo$NI)
  Viajes_NE_NE_costo$NJ <-sub("^", "E", Viajes_NE_NE_costo$NJ)
  
  # se calcula el costo para los arcos NE ND
  
  Viajes_NE_ND_costo <- Viajes_NE_ND_google%>% transmute(NI,NJ,c=(Viajes_NE_ND_google$Tiempo * Costo_Segundo ))
  Viajes_NE_ND_costo$NI <-sub("^", "E", Viajes_NE_ND_costo$NI)
  Viajes_NE_ND_costo$NJ <-sub("^", "D", Viajes_NE_ND_costo$NJ)
  
  
  # Se define los par??metros de los costos de cada arco
  
  Costos <- rbind(VajesSink_costo,Viajes_NO_NE_costo,Viajes_NE_NE_costo,Viajes_NE_ND_costo)
  
  # Se pasan los par??metros del modelo a Excel
  
  write.csv(DemandaViajes,file = paste0(RutaBaseDatos3,"DemandaViajes.csv"))
  write.csv(Arcos,file = paste0(RutaBaseDatos3,"Arcos.csv"))
  write.csv(Costos,file = paste0(RutaBaseDatos3,"Costos.csv"))
  write.csv(Estaciones,file = paste0(RutaBaseDatos3,"Estaciones.csv"))
  
  
  Estaciones_Resultados <- read_excel("USC/Estaciones_Resultados.xlsx")

  # creo archivo con ubicaci??n de las estaciones
  Estaciones_Leaflet<-Estaciones_google%>% filter(NE %in% c(Estaciones_Resultados$NE))%>% mutate(lng=lon)
  Estaciones_Leaflet<- merge(Estaciones_Leaflet, Estaciones_Resultados, by = c('NE'))
  Estaciones_Leaflet$NE<-sub("^", "E", Estaciones_Leaflet$NE)
  Estaciones_Leaflet <- Estaciones_Leaflet %>% transmute(NE,lng,lat,Size)
  
  save(Estaciones_Leaflet,file=paste0(RutaBaseDatos3,"Estaciones_Leaflet.Rdata"))
  write.csv(Estaciones_Leaflet,file = paste0(RutaBaseDatos3,"Estaciones_Leaflet.csv"))
  
  
  EstacionesO <- Estaciones_Leaflet %>% transmute(NI=NE,lng,lat)
  EstacionesD <-Estaciones_Leaflet  %>% transmute(NJ=NE,lng,lat)
  RepresentacionViajes<- Arcos_NE_NE%>% transmute(NI,NJ)
  RepresentacionViajes<- merge(EstacionesO, RepresentacionViajes, by = c('NI'))
  RepresentacionViajes<- merge(EstacionesD, RepresentacionViajes, by = c('NJ'))
  

  ViajesAnalizados<-ViajesPosibles %>% transmute(ID_Viaje,ponderador_calibrado_viajes,hora_inicio,distance,MEDIO_PREDOMINANTE,T)
  save(ViajesAnalizados,file=paste0(RutaBaseDatos3,"ViajesAnalizados.csv"))
  
  write.csv(ViajesAnalizados,"/Users/lukasbogota/Desktop/Analisis de Datos/PG2 Lukas/USC/Modos/ViajesAnalizados.csv", row.names = FALSE)
  
  write.csv(OD,"/Users/lukasbogota/Desktop/Analisis de Datos/PG2 Lukas/USC/Modos/OD.csv", row.names = FALSE)
  
