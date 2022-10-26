library(raster)
library(sf)
library(tidyverse)


###Add data for pixel/pixel validation
#Load 2019 observed land cover
LC2019<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2019.tif")
#Load 2018 observed landcover
LC2018<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2018.tif")
#Make everything that's not Ag 0 
LC2019[]<-ifelse(LC2019[]==2,1,0)
#filter to just NEW ag converted from coral rag from 2018 ->2019
LC2019[]<-ifelse(LC2019[]==1 & LC2018[]==5,1,0)
#Import Pemba Vector
Pemba <- read_sf("LargeABMPolys.shp")


#Do the same for 2019 -> 2021
###Add data for pixel/pixel validation
#Load 2021 observed land cover
LC2021<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2021.tif")
LC2019_2<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2019.tif")
#Make everything that's not Ag 0 
LC2021[]<-ifelse(LC2021[]==2,1,0)
#filter to just NEW ag converted from coral rag from 2018 ->2019
LC2021[]<-ifelse(LC2021[]==1 & LC2019_2[]==5,1,0)


ConvTimeSrs<-data.frame(Area=unique(Pemba$NAME_2),CRPix2018=rep(NA,length(unique(Pemba$NAME_2))),
                        Conv2018_2019=rep(NA,length(unique(Pemba$NAME_2))),
                        Conv2019_2021=rep(NA,length(unique(Pemba$NAME_2))))
#this loop runs the model for each value in the prior and saves the output in the df created above
for(i in 1:nrow(ConvTimeSrs)){
  
  ValidationArea<-filter(Pemba,NAME_2==ConvTimeSrs[i,]$Area)
  
  #get 2018 starting CR pix
  LC2018_clipped<- crop(LC2018, extent(ValidationArea)) #clip the area
  LC2018_clipped <- mask(LC2018_clipped, ValidationArea) #This deals with NAs
  ConvTimeSrs[i,]$CRPix2018<-length(which(LC2018_clipped[]==5))
  
  #Do the 2018 -> 2019 bit
  LC2019_clipped<- crop(LC2019, extent(ValidationArea)) #clip the area
  LC2019_clipped <- mask(LC2019_clipped, ValidationArea) #This deals with NAs
  
  #correct number of conversions
  ConvTimeSrs[i,]$Conv2018_2019<-length(which(LC2019_clipped[] ==1))
  
  #Do the 2019 ->2021 bit
  LC2021_clipped<- crop(LC2021, extent(ValidationArea)) #clip the area
  LC2021_clipped <- mask(LC2021_clipped, ValidationArea) #This deals with NAs
  
  #correct number of conversions
  ConvTimeSrs[i,]$Conv2019_2021<-length(which(LC2021_clipped[] ==1))
  
}

ConvTimeSrs<-ConvTimeSrs%>%mutate(CRPix2019=CRPix2018 -Conv2018_2019)%>%
  mutate(CRPix2021=CRPix2019 -Conv2019_2021)%>%
  dplyr::select(Area,CRPix2018,CRPix2019,CRPix2021)%>%
  pivot_longer(cols=2:4,names_to = "Year",values_to = "Pix")%>%
  mutate(Year=as.integer(gsub("CRPix","",Year)))

ggplot(ConvTimeSrs,aes(x=Year,y=Pix,group=Area))+
  geom_line(aes(color=Area),size=2,alpha=0.6)+
  scale_y_continuous(labels=scales::comma)+theme_classic()+
  ylab("Number of 20mX20m pixels observed")+
  scale_color_viridis_d(option="H")+theme(axis.title = element_text(color="black",size=16),
                                          axis.text = element_text(color="black",size=12),
                                          legend.text =element_text(color="black",size=14))





######################



