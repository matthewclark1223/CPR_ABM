library(raster)
library(sf)
library(tidyverse)
set.seed(1)
LU_AMB(YearsPast2018 = 1, #years (timesteps) to run model
       Wards = c("Kangagani","Kojani"),  #character vector or wards to model. Default is full model
       FallowTime = 3, #time (in years) it takes for fallow land to recharge
       AgLimit = 2)

#Clip to kanganani to get rid of NAs outside
kangagani<-filter(Pemba,NAME_3%in%c("Kangagani","Kojani"))
NewBurn_clipped<- crop(rstack$NEWBurn, extent(kangagani))
NewBurn_clipped <- mask(NewBurn_clipped, kangagani)

#Load observed fire data
Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")
stackRS<-raster::stack("~/Pemba_Project/HCRI_Grant/ProjectFiles/PembaFiresAndPredictors.tif")
fires<-stackRS$PembaFiresAndPredictors.4

#Clip to kanganani
fires_clipped<- crop(fires, extent(kangagani))
fires_clipped <- mask(fires_clipped, kangagani)



p <- rgeos::gBuffer(sampleRandom(fires_clipped, 20, sp=TRUE), byid=TRUE, width=0.005) 
p<-crop(p, extent(kangagani))

x<-st_as_sf(p)

#convert fire data to df for plotting
fires_clipped_df<-raster::as.data.frame(fires_clipped, xy = TRUE) 
names(fires_clipped_df)[3]<-"layer"
fires_clipped_df<-na.omit(fires_clipped_df)

#Convert pred fire data to df for plotting 
NewBurn_df<-raster::as.data.frame(NewBurn_clipped, xy = TRUE) 
NewBurn_df<-na.omit(NewBurn_df)
names(NewBurn_df)[3]<-"layer"

ggplot(data = kangagani)+
  geom_sf(fill=NA)+
  geom_sf(data=x)+
  geom_tile(data=filter(NewBurn_df,layer==1),aes(x=x,y=y),fill="green",alpha=0.5)+
  geom_tile(data=filter(fires_clipped_df,layer==1),aes(x=x,y=y),fill="purple",alpha=0.5)+theme_bw()


( e <- raster::extract(fires_clipped, p) )
( ObsFire.counts <- lapply(e, sum,na.rm=T) ) 
df<-data.frame(Obs=matrix(unlist(ObsFire.counts), nrow=length(ObsFire.counts), byrow=TRUE))

( e <- raster::extract(NewBurn_clipped, p) )
( PredFire.counts <- lapply(e, sum,na.rm=T) ) 
df$Pred<-matrix(unlist(PredFire.counts), nrow=length(PredFire.counts), byrow=TRUE)

ggplot(df,aes(x=Obs,y=Pred))+
  geom_jitter(alpha=0.7)

cor(df$Obs,df$Pred)

#Clip to kanganani
Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/pemmyRF2018_R5.tif")
LC_clipped<- crop(Pem2018LC, extent(kangagani))
LC_clipped <- mask(LC_clipped, kangagani)
LC_clipped_df<-raster::as.data.frame(LC_clipped, xy = TRUE) 
names(LC_clipped_df)[3]<-"layer"

LC_clipped_df<-na.omit(LC_clipped_df)


#plot it
cols <- c("0" = "#c7e9c0", "1" = "#00441b", "2" = "#fdbf6f", "3" = "#4d4d4d",
          "4"= "#ffffbf","5"="lightpink","6"="#41ab5d","7"="#4575b4")
ggplot(data = kangagani)+
  
  geom_tile(data = LC_clipped_df , 
            aes(x = x, y = y,fill=as.character(layer))) +
  geom_tile(data=filter(NewBurn_df,layer==1),aes(x=x,y=y),fill="red")+
  geom_tile(data=filter(fires_clipped_df,layer==1),aes(x=x,y=y),fill="yellow")+
  geom_sf(color="#f0f0f0",fill=NA, size=0.3) + 
  scale_fill_manual(values = cols,labels = c("Mangrove","High Forest",
                                             "Agriculture","Urban","Bare",
                                             "Coral rag","Other woody veg &\nAgroforestry","Water"),
                    name="Landcover")+
  ggtitle("Landcover classification estimates (2018); Kojani")+
  
  theme_bw()



z<-data.frame(real=fires_clipped_df$layer,pred=NewBurn_df$layer)
cor(z$real,z$pred)
ggplot(data=z,aes(x=real,y=pred))+geom_jitter()

sum(z$real)
sum(z$pred)
