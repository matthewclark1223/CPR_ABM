library(sf)
library(tidyverse)


Pemba<-read_sf("Data/PembaShapeFile.shp")

PartDat<-read_sf("CompleteGriddedData.shp")
names(PartDat)<-c("CellNumber","Mangrove","SumValue","MeanValue","MedValue",
                  "Biomass","SatelliteSUM","SatelliteMEAN","geometry")




#prep for prioritizer
PartDat$Cost<-rep(0,nrow(PartDat))
PartDat$Cost<-ifelse(is.na(PartDat$Mangrove),NA,PartDat$Cost)
PartDat$Cost<-ifelse(PartDat$Mangrove==0,NA,PartDat$Cost)


#Make a raster of the satellite observed change..Doing SUM here
Satellite_rast<-as(stars::st_rasterize(PartDat %>% dplyr::select(SatelliteSUM, geometry) ),"Raster")

#Make a raster of the biomass product
Biomass_rast<-as(stars::st_rasterize(PartDat %>% dplyr::select(Biomass, geometry) ),"Raster")

#Make a raster of mangrove presence (community reported)
Mang_rast<-as(stars::st_rasterize(PartDat %>% dplyr::select(Mangrove, geometry)),"Raster")

#Make a raster of the mean community reported change
ComChang_rast<-as(stars::st_rasterize(PartDat %>% dplyr::select(MeanValue, geometry)),"Raster")

#Make a raster of the cost layer (all 0 for now)
Cost_rast<-as(stars::st_rasterize(PartDat %>% dplyr::select(Cost, geometry)),"Raster")

#Lock out everything that's not mangrove
#PartDat$LockedOut<-ifelse(PartDat$Mangrove==1,FALSE,TRUE)
#PartDat$LockedOut<-ifelse(is.na(PartDat$LockedOut),TRUE,PartDat$LockedOut)

#Alternatively, lock out everywhere we didn't sample!
PartDat$LockedOut<-ifelse(is.na(PartDat$MedValue),FALSE,TRUE)




#run prioritizer
library(prioritizr)

Satellite_rast[]<-ifelse(is.na(ComChang_rast[]),NA,Satellite_rast[])





##### Try scaling them the same!
ComChang_rast[]<-(scale(ComChang_rast[])+3.5)
Satellite_rast[]<-scale(Satellite_rast[])+3.5

plot(ComChang_rast)
plot(Satellite_rast)





#stack the predictors  ##Don't include biomass

stack<-raster::stack(ComChang_rast)

######


p1<-problem(Cost_rast,stack)%>%  #first is cost, second is conservation feature. Can be stack
  add_min_set_objective() %>%
  #add_relative_targets(0.05) %>% #percent of cons feature
  add_absolute_targets(2000)%>% #total number of pixels
  add_binary_decisions()%>% #leave in, this is the default of "conserving" or not
  add_boundary_penalties(penalty=0.001,edge_factor=0.25)%>% #seems like penalty is on the scale of the data
  add_gurobi_solver(gap = 0.1, presolve = 2, time_limit = 5)

s1<-solve(p1)
plot(s1)

eval_n_summary(p1,s1)

s1_df<-raster::as.data.frame(s1, xy = TRUE)
names(s1_df)[3]<-"layer"
s1_df$layer<-ifelse(s1_df$layer==0,NA,s1_df$layer)
s1_df<-na.omit(s1_df)

both<-ggplot(data=Pemba)+geom_sf(color="grey",fill="white")+
  geom_tile(data = s1_df, aes(x = x, y = y,fill=as.character(layer)),alpha=0.99)+
  scale_fill_manual(values =c("#238b45"),name="Conservation\npriority areas",
                    labels=c(""))+
  theme_void()+
  theme(panel.background = element_rect(fill="white",color="white"),
        plot.background = element_rect(fill="white",color="white"))+
  theme(legend.position="none")



####
#Now Satellite Only
#stack the predictors  ##Don't include biomass
stack<-raster::stack(Satellite_rast)

######


p1<-problem(Cost_rast,stack)%>%  #first is cost, second is conservation feature. Can be stack
  add_min_set_objective() %>%
  #add_relative_targets(0.05) %>% #percent of cons feature
  add_absolute_targets(2000)%>% #total number of pixels
  add_binary_decisions()%>% #leave in, this is the default of "conserving" or not
  add_boundary_penalties(penalty=0.001,edge_factor=0.25)%>% #seems like penalty is on the scale of the data
  add_gurobi_solver(gap = 0.1, presolve = 2, time_limit = 5)

s1<-solve(p1)
plot(s1)

eval_n_summary(p1,s1)

s1_df<-raster::as.data.frame(s1, xy = TRUE)
names(s1_df)[3]<-"layer"
s1_df$layer<-ifelse(s1_df$layer==0,NA,s1_df$layer)
s1_df<-na.omit(s1_df)

both<-ggplot(data=Pemba)+geom_sf(color="grey",fill="white")+
  geom_tile(data = s1_df, aes(x = x, y = y,fill=as.character(layer)),alpha=0.99)+
  scale_fill_manual(values =c("#238b45"),name="Conservation\npriority areas",
                    labels=c(""))+
  theme_void()+
  theme(panel.background = element_rect(fill="white",color="white"),
        plot.background = element_rect(fill="white",color="white"))+
  theme(legend.position="none")


####
#Now Both!
#stack the predictors  ##Don't include biomass
stack<-raster::stack(ComChang_rast,Satellite_rast)

######


p1<-problem(Cost_rast,stack)%>%  #first is cost, second is conservation feature. Can be stack
  add_min_set_objective() %>%
  add_relative_targets(0.2) %>% #percent of cons feature
  #add_absolute_targets(2500)%>% #total number of pixels
  add_binary_decisions()%>% #leave in, this is the default of "conserving" or not
  add_boundary_penalties(penalty=0.001,edge_factor=0.25)%>% #seems like penalty is on the scale of the data
  add_gurobi_solver(gap = 0.1, presolve = 2, time_limit = 5)

s1<-solve(p1)
plot(s1)

eval_n_summary(p1,s1) #683

s1_df<-raster::as.data.frame(s1, xy = TRUE)
names(s1_df)[3]<-"layer"
s1_df$layer<-ifelse(s1_df$layer==0,NA,s1_df$layer)
s1_df<-na.omit(s1_df)

both<-ggplot(data=Pemba)+geom_sf(color="grey",fill="white")+
  geom_tile(data = s1_df, aes(x = x, y = y,fill=as.character(layer)),alpha=0.99)+
  scale_fill_manual(values =c("#238b45"),name="Conservation\npriority areas",
                    labels=c(""))+
  theme_void()+
  theme(panel.background = element_rect(fill="white",color="white"),
        plot.background = element_rect(fill="white",color="white"))+
  theme(legend.position="none")
