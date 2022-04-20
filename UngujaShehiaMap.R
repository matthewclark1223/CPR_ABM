library(tidyverse)
library(sf)
gadm3_3 <- readRDS("~/Pemba_Project/MapData/gadm36_TZA_3_sf.rds")
ggplot() + geom_sf(data = gadm3_3) + theme_bw()
Unguja_vector <- st_crop(gadm3_3, xmin = 39.1, xmax = 41.0,
                        ymin = -6.5, ymax = -5.5)

Unguja_vector = sf::st_cast(Unguja_vector, "MULTIPOLYGON")

p<- ggplot() + geom_sf(data = Unguja_vector,aes(fill=NAME_2)) + 
  geom_sf_text(data=Unguja_vector,aes(label=NAME_3),size=3)+ theme_bw()

data<-filter(Unguja_vector,NAME_2%in% c("Kusini","Kati"))
ggplot(data = data) + geom_sf(aes(fill=NAME_2)) + 
  geom_sf_text(data=data,aes(label=NAME_3),size=3)+ theme_bw()

plotly::ggplotly(p) %>%
  #highlight(
  # "plotly_hover",
  # selected = attrs_selected(line = list(color = "black"))
  #  ) %>%
  widgetframe::frameWidget()
