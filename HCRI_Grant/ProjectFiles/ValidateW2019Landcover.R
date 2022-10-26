library(raster)
library(sf)
library(tidyverse)
set.seed(1)
source("~/Pemba_Project/HCRI_Grant/ProjectFiles/Full_LandUse_ABM.R")

studyShehia<-c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
               "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
               "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi")

studyShehia<-"Dodo"

LU_AMB(YearsPast2018 = 1, #years (timesteps) to run model
       Wards = c(studyShehia),  #character vector or wards to model. Default is full model
       FallowTime = 3, #time (in years) it takes for fallow land to recharge
       AgLimit = 2,
       IntrinsicExp = 1.5)

Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")

#Clip to Project area to get rid of NAs outside
ValidationArea<-filter(Pemba,NAME_3%in%c(studyShehia))
NewBurn_clipped<- crop(rstack$NEWBurn, extent(ValidationArea))
NewBurn_clipped <- mask(NewBurn_clipped, ValidationArea)

#Load 2019 landcover

LC2019<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2019.tif")
#Load 2018 landcover
LC2018<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2018.tif")
#Make everything that's not Ag 0
LC2019[]<-ifelse(LC2019[]==2,1,0)

#filter to just NEW ag converted from coral rag from 2018 ->2019
LC2019[]<-ifelse(LC2019[]==1 & LC2018[]==5,1,0)



#Clip to kanganani
LC2019_clipped<- crop(LC2019, extent(ValidationArea))
LC2019_clipped <- mask(LC2019_clipped, ValidationArea)



p <- rgeos::gBuffer(sampleRandom(LC2019_clipped, 500, sp=TRUE), byid=TRUE, width=0.004)#50,6 
p<-crop(p, extent(ValidationArea))

x<-st_as_sf(p)

#convert fire data to df for plotting
LC2019_clipped_df<-raster::as.data.frame(LC2019_clipped, xy = TRUE) 
names(LC2019_clipped_df)[3]<-"layer"
LC2019_clipped_df<-na.omit(LC2019_clipped_df)

#Convert pred fire data to df for plotting 
NewBurn_df<-raster::as.data.frame(NewBurn_clipped, xy = TRUE) 
NewBurn_df<-na.omit(NewBurn_df)
names(NewBurn_df)[3]<-"layer"

ggplot(data = ValidationArea)+
  geom_sf(fill=NA)+
  #geom_sf(data=x)+
  geom_tile(data=filter(LC2019_clipped_df,layer==1),aes(x=x,y=y),fill="purple",alpha=0.5)+
  geom_tile(data=filter(NewBurn_df,layer==1),aes(x=x,y=y),fill="red",alpha=0.5)+
  theme_bw()


( e <- raster::extract(LC2019_clipped, p) )
( ObsConversion.counts <- lapply(e, sum,na.rm=T) ) 
df<-data.frame(Obs=matrix(unlist(ObsConversion.counts), nrow=length(ObsConversion.counts), byrow=TRUE))

( e <- raster::extract(NewBurn_clipped, p) )
( PredConversion.counts <- lapply(e, sum,na.rm=T) ) 
df$Pred<-matrix(unlist(PredConversion.counts), nrow=length(PredConversion.counts), byrow=TRUE)

ggplot(df,aes(x=Obs,y=Pred))+
  geom_jitter(alpha=0.7)

cor(df$Obs,df$Pred)

#Clip to kanganani
Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2018.tif")
LC_clipped<- crop(Pem2018LC, extent(ValidationArea))
LC_clipped <- mask(LC_clipped, ValidationArea)
LC_clipped_df<-raster::as.data.frame(LC_clipped, xy = TRUE) 
names(LC_clipped_df)[3]<-"layer"

LC_clipped_df<-na.omit(LC_clipped_df)


#plot it
cols <- c("0" = "#c7e9c0", "1" = "#00441b", "2" = "#fdbf6f", "3" = "#4d4d4d",
          "4"= "#ffffbf","5"="lightpink","6"="#41ab5d","7"="#4575b4")
ggplot(data = ValidationArea)+
  
  geom_tile(data = LC_clipped_df , 
            aes(x = x, y = y,fill=as.character(layer))) +
  geom_tile(data=filter(NewBurn_df,layer==1),aes(x=x,y=y),fill="red")+
  geom_tile(data=filter(LC2019_clipped_df,layer==1),aes(x=x,y=y),fill="yellow",alpha=0.6)+
  geom_sf(color="#f0f0f0",fill=NA, size=0.3) + 
  scale_fill_manual(values = cols,labels = c("Mangrove","High Forest",
                                             "Agriculture","Urban","Bare",
                                             "Coral rag","Other woody veg &\nAgroforestry","Water"),
                    name="Landcover")+
  ggtitle("Landcover classification estimates (2018); ValidationArea")+
  
  theme_bw()




