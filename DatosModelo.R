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

#Defino la ruta de los archivos con los cuales voy a trabajar

RutaBaseDatos1<-"/Users/lukasbogota/Desktop/PG2/Datos Lukas PG2/"
RutaBaseDatos2<-"/Users/lukasbogota/Desktop/"

#Se determinan las franjas horarias a tener en cuenta
InicioF1 <- hm("05:30")
InicioF2 <- hm("08:30")
InicioF3 <- hm("11:30")
InicioF4 <- hm("13:30")
InicioF5 <- hm("16:30")
InicioF6 <- hm("19:30")
FinF6 <- hm("22:00")

#Cargo información
  
  #Cargo la información de los viajes de No bicicleta

  load(paste0(RutaBaseDatos1, "viajes.Rdata")) 
  
    #Se cambia el Id de los viajes por el #de columna
  
    viajes <- viajes %>% rename(c(id="ID_Viaje")) %>% mutate(ID_Viaje=row_number())
  
    #Filtro los datos para sólo tener en cuenta viajes de menos de 13 km de longitud y asigno franja horaria
    
    viajes <- viajes %>% filter(distance < 13e3) %>%
      mutate(T = case_when((hora_inicio > InicioF1) & (hora_inicio < InicioF2) ~ 1,
                           hora_inicio > InicioF2 & hora_inicio < InicioF3 ~ 2,
                           hora_inicio > InicioF3 & hora_inicio < InicioF4 ~ 3,
                           hora_inicio > InicioF4 & hora_inicio < InicioF5 ~ 4,
                           hora_inicio > InicioF5 & hora_inicio < InicioF6 ~ 5,
                           hora_inicio > InicioF6 & hora_inicio < FinF6 ~ 6,
                           TRUE ~ 0)) %>% filter(T != 0)

  #Cargo la información de la ubicación de los parqueaderos públicos en Bogotá
  load(paste0(RutaBaseDatos1, "parqueaderos.Rdata"))

  parqueaderos<-rename(parqueaderos,c("ID_Parquedero"="ID_Parqueadero"))

  #Cargo el layer de las localidades y los transformo a las coordenadas deseadas. 

  LayerLocalidad<-st_read(paste0(RutaBaseDatos2,"Mapas de Referencia IDECA/MR0318.gdb"),layer = "Loca",stringsAsFactors = FALSE) %>% st_transform(4326)


  
#Se filtran los parqueaderos que se encuentran en las localidades deseadas

  #Identifico la localidad de Usaquen y Chapinero y transformo las coordenadas
  LayerLocalidad <- LayerLocalidad %>% filter(LocNombre %in% c("USAQUEN", "CHAPINERO")) %>% st_transform(3116)


  #Filtro los parqueaderos por localidad, borrando aquellos que no pertenencen y asigno como ID el número de columna

  parqueaderos <- parqueaderos %>% st_join(select(LayerLocalidad, LocNombre)) %>% filter(!is.na(LocNombre)) %>% transmute(ID_Parqueadero=row_number())


  
#Se establecen como posibles estaciones a aquellos parqueaderos que tengan un nodo de origen o un nodo de destino a menos de 500 mts
  
  #Se crea una capa con los origenes, y se tranforma a las coordenadas deseadas
  viajesOrigen <- viajes %>% st_as_sf(coords=c(7,6))  %>% transmute(ID_Viaje)  %>% mutate(ID_O= ID_Viaje) %>%  st_set_crs(4326) %>% st_transform(3116)
  
  #Se crea una capa con los destinos, y se tranforma a las coordenadas deseadas
  viajesDestino<- viajes %>% st_as_sf(coords=c(9,8)) %>% transmute(ID_Viaje) %>% mutate(ID_D= ID_Viaje) %>%  st_set_crs(4326)%>% st_transform(3116)
  
  #Se identifica si el parqueadero sirve para cubrir algún viaje
  Estaciones <- parqueaderos %>% st_join(select(st_buffer(viajesOrigen, dist=500),ID_O),largest = TRUE) 
  Estaciones <- Estaciones %>% st_join(select(st_buffer(viajesDestino, dist=500),ID_D),largest = TRUE) 
  Estaciones <- filter(Estaciones,(!is.na(ID_O) && !is.na(ID_D)))
  
  #Se obtiene el conjunto de estaciones resultantes
  Estaciones <- Estaciones %>% transmute(NE=ID_Parqueadero)
  Estaciones$NE<-sub("^", "E", Estaciones$NE)
  

#Se verifica si un viaje (par origen-destino) puede ser realizado
  
  # se identifica y se filtran los viajes que desde el origen tienen una estación a menos de 500mts
  viajesOrigen<- viajesOrigen %>% st_join(st_buffer(Estaciones, dist=500),largest = TRUE) %>% filter(!(is.na(NE))) 
  
  # se identifica y se filtran los viajes que hacia el destino tienen una estación a menos de 500mts
  viajesDestino<- viajesDestino %>% st_join(st_buffer(Estaciones, dist=500),largest = TRUE) %>% filter(!(is.na(NE))) 

  # Se filtran los viajes que cumplan ambas condiciones
  ViajesPosibles<-viajes%>% left_join(st_set_geometry(viajesOrigen,NULL)) %>% filter(!(is.na(NE))) %>% subset(select = -c(NE) )
  ViajesPosibles<-ViajesPosibles%>% left_join(st_set_geometry(viajesDestino,NULL)) %>% filter(!(is.na(NE))) %>% subset(select = -c(NE) )
  ViajesPosibles <- ViajesPosibles %>% mutate(ID_Viaje=row_number())

  rm(viajes,viajesDestino,viajesOrigen,parqueaderos,LayerLocalidad)  

  
#Se identifican los nodos de origen

NodosOrigen <- ViajesPosibles %>% st_as_sf(coords=c(7,6)) %>% transmute(ID_Viaje) %>%  st_set_crs(4326) %>% st_transform(3116)
NodosOrigen<-NodosOrigen%>% mutate(NO= sub("^", "O", NodosOrigen$ID_Viaje ))
  
#Se identifican los nodos de destino

NodosDestino <- ViajesPosibles %>% st_as_sf(coords=c(9,8)) %>% transmute(ID_Viaje) %>%  st_set_crs(4326) %>% st_transform(3116)
NodosDestino<-NodosDestino%>% mutate(ND=sub("^", "D", NodosDestino$ID_Viaje))

  
#Se identifican los posibles arcos del sistema

  # Arcos desde el nodo de origen hacia la estación de préstamo 
  
  Arcos_NO_NE <- NodosOrigen %>% st_join(st_buffer(Estaciones, dist=500))  %>% rename(c(NE="NE_O")) 
  rownames(Arcos_NO_NE) <- NULL
  
  #Arcos desde la estación de devolución hasta el nodo de destino
  
  Arcos_NE_ND <- NodosDestino %>% st_join(st_buffer(Estaciones, dist=500))  %>% rename(c(NE="NE_D"))
  rownames(Arcos_NE_ND) <- NULL
  
  #Arcos entre las estaciones de préstamo y de entrega (para cada posible viaje)
  Arcos_NE_NE <- Arcos_NO_NE %>% st_set_geometry(NULL) %>% left_join(Arcos_NE_ND %>% st_set_geometry(NULL))
  
  
  #Acomodo los arcos al formato del modelo de Pyomo
  
  Arcos_NE_NE <- Arcos_NE_NE %>% transmute(NO,ND,NI=NE_O,NJ=NE_D)
  
  
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
  
  
  
  
  
  