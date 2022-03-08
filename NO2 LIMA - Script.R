##--------------------------
library(sf)
library(mapedit)
library(raster)
library(ggplot2)
library(tmap)
library(rgee)
library(mapedit)
library(rgee)
library(googledrive)
library(rgee)
library(mapedit)
library(tibble)
library(sf)
library(cptcity)
library(tidyverse)
library(sp)
library(leaflet.extras2)
library(raster)
library(stars)
library(geojsonio)
##--------------------------
ee_Initialize("gflorezc", drive = T)

box <- ee$Geometry$Rectangle(coords= c(-77.26903, -12.52042, -76.62148,-11.57288),
                             proj= "EPSG:4326", geodesic = F)

# Comportamiento de N02 - Sentinel 5p

## Llamar a Sistema de vigilancia de calidad de Aire
Poligono <-ee$FeatureCollection("users/gflorezc/Lima_Province")
PaleN02020 <- c('#9331dc', '#165dff', '#10aebe', '#00ffff', '#ffee21', '#f19e21', '#ff4223')
##--------------------------
# Periodo 2020 - 
N02018 = ee$ImageCollection("COPERNICUS/S5P/OFFL/L3_NO2")$  # NO2
  select("tropospheric_NO2_column_number_density")$ #Datos para la columna de NO2
  filterDate("2018-01-01", "2019-01-01")$
  median()$
  clip(Poligono)

# Periodo 2020 - 
N02020 = ee$ImageCollection("COPERNICUS/S5P/OFFL/L3_NO2")$  # NO2
  select("tropospheric_NO2_column_number_density")$ #Datos para la columna de NO2
  filterDate("2020-01-01", "2021-01-01")$
  median()$
  clip(Poligono)
  
# Visualizacion
Map$centerObject(Poligono)
Map$addLayer(N02018, visParams = list(                      
  min=0,   max=0.0002, palette= PaleN02020) ,0.6, TRUE ,name="NO2 2020-2021")+
  
Map$addLegend(visParams = list(min=0,   max=0.0002, palette= PaleN02020),
              name = "NO2 mol/m2 (2018)" ,position = c("bottomleft") )         |
  
  Map$addLayer(N02020, visParams = list(                      
    min=0,   max=0.0002, palette= PaleN02020) ,0.6, TRUE ,name="NO2 2018-2019")+
  Map$addLegend(visParams = list(min=0,   max=0.0002, palette= PaleN02020),
                name = "NO2 mol/m2 (2020)" ,position = c("bottomright") )        

#  Exportacion
N02018 %>% ee_as_raster(region= box, scale=10, dsn="N02018/N0201") -> Clasi_paul
N02020 %>% ee_as_raster(region= box, scale=10, dsn="N02020/N02020") -> Clasi_paul
##--------------------------

Clas_2018<- raster("Raster/N0201_2022_03_08_17_17_07.tif")
Clas_2020<- raster("Raster/N02020_2022_03_08_17_53_41.tif")

Clas.pa        <-  rasterToPoints(Clas)
Clas.pa_a      <-  data.frame(Clas.pa)
colnames(Clas.pa_a) = c("lon", "lat", "NO2")
summary(Clas.pa_a)

ggplot() +
  geom_raster(data = Clas.pa_a , aes(lon,lat,fill =NO2)) +
  scale_fill_gradientn(colours = PaleN02020, name="Leyenda \nTipos",
                       labels = c("[Bosque] ","[Suelo desnudo]", 
                                  "[Tierra estéril]", "[Bosque secundario]", "[Agricultura]"),
                       breaks = c(1,2,3,4,5))

Peru   <- getData('GADM', country='Peru', level=3) %>% st_as_sf()
Lima      <- subset(Peru, NAME_1  == "Lima Province")



p1= tm_shape(Clas_2018) +
  tm_raster(palette = PaleN02020, n = 20, alpha=0.8, title = "NO2 mol/m2 (2018)")+
  tm_shape(Lima)+
  tm_borders("white",lwd=2)+
  tm_text("NAME_3",size = .8, col="black",shadow=TRUE,fontfamily = "Kefa", 
          fontface = "bold",
          bg.color="white", bg.alpha=.35)+
  
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "white", color.dark = "lightsteelblue4", 
               position = c(.01, 0.005), lwd = 1, color.light= "white")+
  tm_compass(type="rose", position=c(.86, 0.05), text.color = "white")+
  tm_layout( title = "Google Earth Engine \nRGEE, \nCOPERNICUS/S5P/OFFL/L3_NO2",
             bg.color="#022140", 
             legend.bg.color = "#416076", 
             title.color  = "white",
             title.size = .9,
             legend.title.size=.8,
             legend.position = c(0.005,0.10) , scale=0.61, legend.frame = T,
             fontface="bold",
             legend.format = c(text.align = "right", 
                               text.separator = "-"))+
  tm_credits("Lima Provincia: Sentinel 5p \n     Calidad del aire 2018", position = c(.58, .9), col = "white", fontface="bold", size=2, fontfamily = "serif")+
  tm_credits("Data: https://www.https://code.earthengine.google.com/ \n#Aprende R desde Cero Para SIG \nIng. Gorky Florez Castillo", position = c(0.1, .04), col = "white", fontface="bold")+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 3, position = c(0.40, 0.05))

p2=tm_shape(Clas_2020) +
  tm_raster(palette = PaleN02020, n = 20, alpha=0.8, title = "NO2 mol/m2 (2020)")+
  tm_shape(Lima)+
  tm_borders("white",lwd=2)+
  tm_text("NAME_3",size = .8, col="black",shadow=TRUE,
          bg.color="white", bg.alpha=.35)+
  tm_scale_bar(width = 0.25, text.size = 0.5, text.color = "white", color.dark = "lightsteelblue4", 
               position = c(.01, 0.005), lwd = 1, color.light= "white")+
  tm_compass(type="rose", position=c(.86, 0.05), text.color = "white")+
  tm_layout( title = "Google Earth Engine \nRGEE, \nCOPERNICUS/S5P/OFFL/L3_NO2",
             bg.color="#022140", 
             legend.bg.color = "#416076",
             title.color  = "white",
             title.size = .9,
             legend.title.size=.8,
             legend.position = c(0.005,0.10) , scale=0.70, legend.frame = T,
             fontface="bold",
             
             legend.format = c(text.align = "right", 
                               text.separator = "-"))+
  tm_credits("Lima Provincia: Sentinel 5p \n     Calidad del aire 2020", position = c(.58, .9), col = "white", fontface="bold", size=2, fontfamily = "serif")+
  tm_credits("Data: https://www.https://code.earthengine.google.com/ \n#Aprende R desde Cero Para SIG \nIng. Gorky Florez Castillo", position = c(0.1, .04), col = "white", fontface="bold")+
  tm_logo(c("https://www.r-project.org/logo/Rlogo.png",
            system.file("img/tmap.png", package = "tmap")),height = 3, position = c(0.40, 0.05))
Mapa =tmap_arrange(p1,p2,nrow=1)

tmap_save(Mapa, "Mapa/NO2_Lima.png", dpi = 1200, width = 13, height = 9)
