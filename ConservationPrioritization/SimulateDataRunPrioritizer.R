library(sf)
library(tidyverse)


Pemba<-read_sf("Data/PembaShapeFile.shp")
Net<-read_sf("Data/IdentifiedMangroves.shp")
Net$Mangrove<-as.logical(Net$Mangrove)

crs(Pemba)

#Sumulate Community Change Data
set.seed(1)
Net$ComChangeSum<-sample(-10:10,prob=(rbeta(21,4,4)),replace=TRUE,size=nrow(Net))
Net$ComChangeSum<-ifelse(is.na(Net$Mangrove),NA,Net$ComChangeSum)
Net$ComChangeSum<-ifelse(Net$Mangrove==0,NA,Net$ComChangeSum)




ggplot(data=Pemba)+geom_sf(color="black")+geom_sf(data=Net,aes(fill=Mangrove),alpha=0.5)+
  scale_fill_manual(values =c("white","green"))+theme_void()+
  theme(panel.background = element_rect(fill="white",color="white"),
        plot.background = element_rect(fill="white",color="white"))

ggplot(data=Pemba)+geom_sf(color="black")+geom_sf(data=Net,aes(fill=ComChangeSum),alpha=0.85)+
  scale_fill_viridis_b(option="mako")+theme_void()+
  theme(panel.background = element_rect(fill="white",color="white"),
        plot.background = element_rect(fill="white",color="white"))


#Simulate change in Satellite observed tree cover
Net$EVI<-runif(nrow(Net),0,1)
Net$EVI<-ifelse(is.na(Net$Mangrove),NA,Net$EVI)
Net$EVI<-ifelse(Net$Mangrove==0,NA,Net$EVI)


ggplot(data=Pemba)+geom_sf(color="black")+geom_sf(data=Net,aes(fill=EVI),alpha=0.85)+
  scale_fill_viridis_c(option="magma")+theme_void()+
  theme(panel.background = element_rect(fill="white",color="white"),
        plot.background = element_rect(fill="white",color="white"))


Net$Cost<-runif(1,100,n=nrow(Net))
Net$Cost<-ifelse(is.na(Net$Mangrove),NA,Net$Cost)
Net$Cost<-ifelse(Net$Mangrove==0,NA,Net$Cost)

EVI_rast<-as(stars::st_rasterize(Net %>% dplyr::select(EVI, geometry) ),"Raster")
Mang_rast<-as(stars::st_rasterize(Net %>% dplyr::select(Mangrove, geometry)),"Raster")
ComChang_rast<-as(stars::st_rasterize(Net %>% dplyr::select(ComChangeSum, geometry)),"Raster")
Cost_rast<-as(stars::st_rasterize(Net %>% dplyr::select(Cost, geometry)),"Raster")
Net$LockedOut<-ifelse(Net$Mangrove==1,FALSE,TRUE)
Net$LockedOut<-ifelse(is.na(Net$LockedOut),TRUE,Net$LockedOut)

library(prioritizr)

p1<-problem(Cost_rast,EVI_rast)%>%  #first is cost, second is conservation feature. Can be stack
  add_min_set_objective() %>%
 # add_relative_targets(0.1) %>% #percent of cons feature
  add_absolute_targets(2000)%>% #total number of pixels
  add_binary_decisions()%>% #leave in, this is the default of "conserving" or not
  add_boundary_penalties(penalty=100000,edge_factor=0.5)%>%
  add_gurobi_solver(gap = 0.1, presolve = 2, time_limit = 5)

s1<-solve(p1)
plot(s1)

eval_n_summary(p1,s1)

s1_df<-raster::as.data.frame(s1, xy = TRUE)
names(s1_df)[3]<-"layer"
s1_df$layer<-ifelse(s1_df$layer==0,NA,s1_df$layer)
s1_df<-na.omit(s1_df)

ggplot(data=Pemba)+geom_sf(color="black")+
  geom_tile(data = s1_df, aes(x = x, y = y,fill=as.character(layer)),alpha=0.75)+
  scale_fill_manual(values =c("green"),name="Conservation priority areas")+
  theme_void()+
  theme(panel.background = element_rect(fill="white",color="white"),
        plot.background = element_rect(fill="white",color="white"))
  
  
  
  