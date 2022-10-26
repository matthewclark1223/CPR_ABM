library(gganimate)
library(sf)
library(raster)
library(tidyverse)

setwd("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandUsePredictionsApp")

studyShehia<-c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
               "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
               "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi")

Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")
Roads<-read_sf("~/Pemba_Project/PembaRoads.shp")

fallowTime<-c(2,3,4)
productiveTime<-c(1,2,3)

df<-expand.grid(studyShehia,fallowTime,productiveTime)
names(df)[1:3]<-c("Shehia","fallowTime","productiveTime")
df$filePath<-paste0("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandUsePredictionsApp/Gifs/",
                    df$Shehia,"_",df$fallowTime,"_",df$productiveTime,".gif")

df$filePath2<-paste0("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandUsePredictionsApp/Plots/",
                     df$Shehia,"_",df$fallowTime,"_",df$productiveTime,".png")



############Now run the models

Pemba <- read_sf("PembaShapeFile.shp")

  
  
  
  replacefun<-function(r){
    r[]<-ifelse(LC2021[] == 0,0,r[]) #Mangrove
    r[]<-ifelse(LC2021[] == 1,10,r[]) #High Forest            
    r[]<-ifelse(LC2021[] == 3,30,r[]) #Urban
    r[]<-ifelse(LC2021[] == 4,40,r[]) #Bare
    r[]<-ifelse(LC2021[] == 6,60,r[]) #OWV/agroforestry
    r[]<-ifelse(LC2021[] == 7,70,r[])#H20
    r[]<-ifelse(r[]==3,1,r[]) #Make only one Ag class
    return(r[])} 
  
  #Extrinsic growth rates
  rates<-data.frame(Shehia=c("Shamiani","Jombwe","Dodo","Mjini Ole","Pujini","Muambe","Kiwani", 
                             "Ole","Mvumoni","Chambani","Uwandani","Shumba Mjini","Kojani","Kibokoni",    
                             "Vitongoji","Fundo","Kangagani","Mjini Wingwi","Maziwa Ng'ombe"),
                    Rate=c( 0,0,0.65,3.33,1.37,0,0.29,1.44,0.65,
                            0.58,1.64,0.86,1.18,0.27,1.12,0,2.53,0.74,3.9))
  
  source("ABM_For_App2.R")
  

for(i in 1:nrow(df)){
  LC2021<-raster::raster("pemmyLC2021updateSAR.tif")
  LU_AMB(YearsPast2021 = 4, #years (timesteps) to run model
         Wards = df[i,]$Shehia ,  #character vector or wards to model. Default is full model
         FallowTime = df[i,]$fallowTime, #time (in years) it takes for fallow land to recharge
         AgLimit = df[i,]$productiveTime,
         IntrinsicExp=rates[rates$Shehia==df[i,]$Shehia,]$Rate)
  
  YearsPast2021=4
  LndCvr<-raster::stack(rstack[[c(2,24:27)]])
  
  LC2021<- crop(LC2021, extent(LndCvr$LndCvr2021))
  LC2021 <- mask(LC2021, LndCvr$LndCvr2021)
  
  
  
  
  
  
  
  for(n in 1:length(names(LndCvr))){
    LndCvr[[n]]<-replacefun(LndCvr[[n]])# do it for all layers in Raster stack (all years)
  }
  
  
  
  z<-list()
  for(l in 1:length(names(LndCvr))){
    d<-raster::as.data.frame(LndCvr[[l]],xy=TRUE)
    names(d)[3]<-"layer"
    d$year<-names(LndCvr[[l]])
    d<-na.omit(d)
    z[[l]]<-d
    
  }
  
  z<-do.call(rbind, z)
  

  
  shehia<-filter(Pemba,NAME_3==df[i,]$Shehia)
  Roads2<-st_intersection(Roads,shehia)

  cols <- c("0" = "#c7e9c0", "10" = "#00441b", "2" = "#e7298a", "30" = "#4d4d4d",
            "40"= "#ffffbf","1"="#fdbf6f","60"="#41ab5d","70"="#4575b4")
  
  plot<- ggplot(data = shehia)+
    geom_sf(color="grey", alpha=0.001 )+
    geom_tile(data=z,aes(x = x, y = y,fill=as.character(layer))) +
    geom_sf(data=Roads2,color="black", size=1)+
    scale_fill_manual(values = cols,labels = c("Mangrove","High Forest","Coral rag forest","Urban","Bare",
                                               "Agriculture","Agroforestry & \nother woody veg", "Water"),
                      name="Landcover")+
    theme_bw()+ labs(title = 'Year: {closest_state}')+theme(axis.text = element_blank())
  
  PATH<-df[i,]$filePath
  
  anim<-plot + 
    transition_states(as.factor(year),transition_length = 0,state_length=1)
  
  anim_save(PATH, gganimate::animate(anim)) # New
  
  
  
  #Now make static plot of burn
  
  
  nbr<-raster::as.data.frame(rstack[["TotalBurn"]],xy=TRUE)
  nbr<-nbr[nbr$TotalBurn==1,]
  nbr<-na.omit(nbr)
  numberConverted<-sum(nbr$TotalBurn)
  nbr$TotalBurn<-"99"
  cols <- c("0" = "#c7e9c0", "10" = "#00441b", "2" = "#e7298a", "30" = "#4d4d4d",
            "40"= "#ffffbf","1"="#fdbf6f","60"="#41ab5d","70"="#4575b4","99"="red")
  
  plot<-ggplot(data = z[z$year=="LndCvr2025",])+
    geom_sf(data=shehia,color="grey",alpha=0.01)+
    geom_sf(data=Roads2,color="black", size=1)+
    geom_tile(aes(x = x, y = y,fill=as.character(layer)),alpha=0.3) +
    geom_tile(data=nbr,aes(x = x, y = y,fill=TotalBurn) )+
    scale_fill_manual(values = cols,labels = c("Mangrove","High Forest","Coral rag forest","Urban","Bare",
                                               "Agriculture","Agroforestry & \nother woody veg", "Water",
                                               "Predicted\ndeforested land"),
                      name="Landcover")+
    theme_void()+ggtitle(paste0(numberConverted, " expected 20mx20m areas converted"))+
    theme(legend.text = element_text(size=15),legend.key.size = unit(0.8, 'cm'),
          legend.title = element_text(size=18),
          plot.title = element_text(size = 30, face = "bold"))
  
PATH2<-df[i,]$filePath2
print(plot)
dev.print(png, file = PATH2, width = 1024, height = 768)

print((i/nrow(df))*100)
remove(rstack)
}

