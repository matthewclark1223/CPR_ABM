library(sf)
library(tidyverse)
Pemba<-read_sf("~/Pemba_Project/PembaShapeFile.shp")

studyShehia<-c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
               "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
               "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi")

d<-Pemba%>%filter(NAME_3 %in% studyShehia)%>%group_by(NAME_2)%>%summarise()
ggplot(data=d)+geom_sf()

sf::st_write(d,"LargeABMPolys.shp") 






