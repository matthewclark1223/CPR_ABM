library(sf)
library(tidyverse)
library(magrittr)
library(rmapshaper)
library(rnaturalearth)
library(rnaturalearthdata)
library(ggspatial)



sizes<-read_csv("~/Pemba_Project/CoFMASizes.csv")
sizes%<>%group_by(CoFMA)%>%mutate(Total=sum(AlternativeUse,ConservationArea,UtilizationArea))%>%
  mutate(UseArea=sum(AlternativeUse,UtilizationArea))%>%
  mutate(UseAreaPerc=UseArea/Total)%>%mutate(ConsAreaPerc=ConservationArea/Total)

sizes%<>%mutate(ChangeYearlyRateForestLoss_FromCofma=-1*(CoverChange_PercPerYear_2010_2018-CoverChange_PercPerYear_2001_2010))

gadm3_3 <- readRDS("~/Pemba_Project/MapData/gadm36_TZA_3_sf.rds")
#ggplot() + geom_sf(data = gadm3_3) + theme_bw()
Pemba <- st_crop(gadm3_3, xmin = 39.5, xmax = 39.9,
                 ymin = -5.6, ymax = -4.75)

#ggplot() + geom_sf(data = Pemba) + theme_bw()
Pemba[Pemba$NAME_3 == "Mgogoni" & Pemba$NAME_2 =="Chake",]$NAME_3<-"Mgogoni_2" #duplicate "mgogni" ward Does NOT have Cofma

CofmaWards<-c("Changaweni", "Fundo", "Gando", "Kambini","Kangani","Kifundi","Kisiwa Panza",
              "Mgelema","Mgogoni","Michenzani","Mjimbini","Mjini Wingwi","Msuka Magharibi","Mtambwe Kaskazini",
              "Mtambwe Kusini","Shumba Mjini","Tondooni","Tumbe Magharibi")


Pemba$CoFMA<-ifelse(Pemba$NAME_3 %in% CofmaWards, "CoFMA","No CoFMA")
sizes$NAME_3<-sizes$CoFMA
outcome<-sizes%>%ungroup()%>%select(NAME_3,ChangeYearlyRateForestLoss_FromCofma)
Pemba<-merge(Pemba,outcome,by="NAME_3",all.x = TRUE)




ggplot() + geom_sf(data = Pemba, aes(fill=ChangeYearlyRateForestLoss_FromCofma))+
  theme_bw() +theme( panel.background = element_rect(fill = "#fff7fb"),panel.grid.major = element_blank(),
                    axis.text=element_blank(), axis.ticks = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.5) +
  scale_fill_gradient2(low = "#01665e", high = "#8c510a", mid = "#f6e8c3",na.value = "#bdbdbd",name="Change in Annual\nForest Cover Loss",
                       breaks=c(5,0,-3), labels = function(x) {ifelse(x>=0,paste0("+",x,"%"),paste0(x,"%"))})




fun<-function(x) {ifelse(x>=0,paste0("+",x,"%"),paste0(x,"%"))}
fun(4)

  


ggplot() + geom_sf(data = Pemba, aes(fill=ChangeYearlyRateForestLoss_FromCofma))+
  theme_bw() +theme( panel.background = element_rect(fill = "white"),panel.grid.major = element_blank(),
                     axis.text=element_blank(), axis.ticks = element_blank())+
  annotation_scale(location = "bl", width_hint = 0.3) +
  annotation_north_arrow(location = "br", which_north = "true", 
                         pad_x = unit(0.1, "in"), pad_y = unit(0.05, "in"), 
                         style = north_arrow_fancy_orienteering)+
  scale_fill_gradient2(low = "#8c510a", high = "#01665e",trans = 'reverse', mid = "#f6e8c3" ,na.value = "#bdbdbd",name="Change in Annual\nForest Cover Loss",
                       breaks=c(-3,0,5), labels = function(x) {ifelse(x>=0,paste0("+",x,"%"),paste0(x,"%"))})






