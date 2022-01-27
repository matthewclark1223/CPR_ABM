library(sf)
library(tidyverse)
PembaBound<-read_sf("~/Pemba_Project/PembaShapeFile.shp")

PembaBound<-PembaBound%>%mutate(StudyCom=ifelse(NAME_3 =="Mjimbini","Yes","No" ))

cols<- c("No" = "#d9d9d9", "Yes" = "#525252")

ggplot()+
  geom_sf(data=PembaBound,aes(fill=StudyCom),color="#bdbdbd")+
  theme_bw()+
  geom_sf_text(data=PembaBound,aes(label=NAME_3),size=1)+
  scale_colour_manual(
    values = cols,
    aesthetics = ( "fill")
  )+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position="none",
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank())+
ggspatial::annotation_north_arrow(location = "bl", which_north = "true",height=unit(1,"cm"), 
                                  width=unit(1,"cm"),
                                  pad_x = unit(0.0, "in"), pad_y = unit(0, "in"), 
                                  style = north_arrow_fancy_orienteering)
