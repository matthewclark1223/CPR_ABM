library(sf)
library(tidyverse)

Pemba<-read_sf("~/Pemba_Project/PembaShapeFile.shp")
#d<-Pemba%>%st_union()
grid<-st_make_grid(Pemba,cellsize = 0.004516827922662469,square=TRUE) #500m at -5degrees
Pem_clip<-Pemba%>%filter(NAME_3=="Ukunjwi")
grid_clip<-grid[Pem_clip]
ggplot(data=Pem_clip)+geom_sf()+geom_sf(data=grid_clip,fill=NA)




grid_df<-st_as_sf(grid_clip)
#grid_df$Value<-(runif(nrow(grid_df),-4,2))
set.seed(1)
grid_df$Value<-sort(rnorm(nrow(grid_df),0,2))
grid_df$Index<-1:nrow(grid_df)
grid_df$Value<-ifelse(grid_df$Index%in%c(69:76,54:63,41:50,27:36,18:22,91:96,81:88),NA,grid_df$Value)

ggplot(data=Pem_clip)+geom_sf()+geom_sf(data=grid_df,aes(fill=Value),alpha=0.5)+
  #geom_sf_label(data=grid_df,aes(label=Value))+
  scale_fill_viridis_c(name="Reported mangrove\nchange", option="magma",
                       breaks=c(2,-4),labels=c("Increasing","Decreasing"),begin=0.1,end=0.9 )+
  theme_bw()+theme(panel.grid = element_blank())


###########
#Turn these data into raster data or not. Prioritizer works with vector data too 

#https://cran.r-project.org/web/packages/prioritizr/vignettes/tasmania.html
