
library(leaflet)
library(dplyr)
library(tidyverse)
library(maps)
library(geosphere)
library(magrittr)

# Brings in the file 'ctlist.csv'
Estaciones <- read.csv("/Users/lukasbogota/Desktop/Analisis de Datos/PG2 Lukas/USC/Modos/Estaciones_Leaflet.csv", stringsAsFactors=FALSE) 


pal <- colorNumeric(
  palette = "Reds",
  domain = Estaciones$Size)

m <- leaflet(Estaciones) %>% addProviderTiles(providers$CartoDB.Positron)
m %>% setView(-74.0641971, 4.6875295, zoom = 8)
m %>% addCircles(~lng, ~lat, popup=Estaciones$type, weight = 3, radius=Estaciones$Size, 
                 color = ~pal(Size), stroke = TRUE, fillOpacity = 0.8) %>%
                 addLegend(pal = pal, values = ~Size, opacity = 0.7, title = "Capacidad estaciones",
                  position = "bottomright")
              

  addLegend("bottomright", colors= "white", labels="Estaciones", title="En Usaquen, Chapinero y Suba")


p1 <- as.matrix(RepresentacionViajes[,c(2,3)]) # it's important to list lng before lat here
p2 <- as.matrix(RepresentacionViajes[,c(5,6)]) # and here

gcIntermediate(p1, p2,  
               n=100, 
               addStartEnd=TRUE,
               sp=TRUE) %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines(color = "orange",weight = 0.4) %>% 
addLegend("bottomright", colors= "orange", labels="Conexiones entre estaciones", title="En Usaquen, Chapinero y Suba")

p3 <- as.matrix(NodosOrigen_google[,c(2,3)]) # it's important to list lng before lat here
p4 <- as.matrix(NodosDestino_google[,c(2,3)]) # and here


gcIntermediate(p3, p4,  
               n=282, 
               addStartEnd=TRUE,
               sp=TRUE) %>% 
  leaflet() %>% 
  addProviderTiles(providers$CartoDB.Positron) %>% 
  addPolylines(color = "red",weight = 0.4) %>% 
  addLegend("bottomright", colors= "red", labels="Lineas de deseo viajes", title="En Usaquen, Chapinero y Suba")


Estaciones_Posibles<-Estaciones_google %>% transmute(NE,lng=lon,lat)

m2 <- leaflet(Estaciones_Posibles) %>% addProviderTiles(providers$CartoDB.Positron)
m2 %>% setView(-74.0641971, 4.6875295, zoom = 8)
m2 %>% addCircles(~lng, ~lat,  weight = 3, radius=10, 
                 color="blue", stroke = TRUE, fillOpacity = 0.8) %>%
  addLegend("bottomright", colors= "blue", labels="Posibles locaciones", title="En Usaquen, Chapinero y Suba")




  

