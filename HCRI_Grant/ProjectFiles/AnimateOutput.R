library(tidyverse)
library(gganimate)
library(gifski)
source("~/Pemba_Project/HCRI_Grant/ProjectFiles/Full_LandUse_ABM.R")
YearsPast2018 = 1
LU_AMB(YearsPast2018 = YearsPast2018, #years (timesteps) to run model
       Wards = c("Kangagani"),  #character vector or wards to model. Default is full model
       FallowTime = 3, #time (in years) it takes for fallow land to recharge
       AgLimit = 2,
       IntrinsicExp = 2)


LndCvr<-raster::stack(rstack[[c(2,(length(names(rstack))-YearsPast2018+1):length(names(rstack)))]])
LC2018<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2018.tif")
LC2018<- crop(LC2018, extent(LndCvr$LndCvr2018))
LC2018 <- mask(LC2018, LndCvr$LndCvr2018)


#Apply this to the function below. Going to have to do something kidm of fucked, but maybe best to just add a single 0
#0 = Mangrove
#1 = HF
#2 = agriculture
#3 = Urban
#4 = Bare
#5 = Coral rag
#6 = OWV/agroforestry
#7 = water





replacefun<-function(r){
r[]<-ifelse(LC2018[] == 0,0,r[]) #Mangrove
r[]<-ifelse(LC2018[] == 1,10,r[]) #High Forest            
r[]<-ifelse(LC2018[] == 3,30,r[]) #Urban
r[]<-ifelse(LC2018[] == 4,40,r[]) #Bare
r[]<-ifelse(LC2018[] == 6,60,r[]) #OWV/agroforestry
r[]<-ifelse(LC2018[] == 7,70,r[])#H20
r[]<-ifelse(r[]==3,1,r[]) #Make only one Ag class
               return(r[])} 



for(i in 1:length(names(LndCvr))){
  LndCvr[[i]]<-replacefun(LndCvr[[i]])# do it for all layers in Raster stack (all years)
}


z<-data.frame(x=NA,y=NA,layer=NA,year=NA)
for(i in 1:length(names(LndCvr))){
  d<-raster::as.data.frame(LndCvr[[i]],xy=TRUE)
  names(d)[3]<-"layer"
  d$year<-names(LndCvr[[i]])
  z<<-rbind(z,d)
  z<-na.omit(z)
  
}


#cols <- c("0" = "#c7e9c0", "10" = "#00441b","1" = "brown", "2" = "orange", "30" = "#4d4d4d",
 #         "40"= "#ffffbf","60"="#41ab5d","70"="#4575b4")

cols <- c("0" = "#c7e9c0", "10" = "#00441b", "2" = "#e7298a", "30" = "#4d4d4d",
          "40"= "#ffffbf","1"="#fdbf6f","60"="#41ab5d","70"="#4575b4")

ggplot(data = filter(z,year=="LndCvr2026"))+
  
  geom_tile(aes(x = x, y = y,fill=as.character(layer))) +
  scale_fill_manual(values = cols,labels = c("Mangrove","High Forest","Coral rag forest","Urban","Bare",
                                             "Agriculture","Agroforestry & \nother woody veg", "Water"),
                    name="Landcover")+
  theme_bw()




library(gganimate)
plot<-ggplot(data = z)+
  
  geom_tile(aes(x = x, y = y,fill=as.character(layer))) +
  scale_fill_manual(values = cols,labels = c("Mangrove","High Forest","Coral rag forest","Urban","Bare",
                                             "Agriculture","Agroforestry & Other woody veg", "Water"),
                    name="Landcover")+
  theme_bw()+ labs(title = 'Year: {closest_state}')
plot + 
  transition_states(as.factor(year),transition_length = 0,state_length=0.5)





