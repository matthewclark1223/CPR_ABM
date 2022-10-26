library(tidyverse)
library(sf)
library(raster)
##2018 to 2019############################################
#Shehia with a considerable amount of coral rag
Pemba <- sf::read_sf("~/Pemba_Project/PembaShapeFile.shp")
studyShehia<-c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
               "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
               "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi")

df<-data.frame(Shehia=studyShehia,PixelsConverted2018_2019=rep(NA,length(studyShehia)))

###Add data for pixel/pixel validation

#Load 2018 observed landcover
LC2018<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/SARLayers/pemmyLC2018updateSAR.tif")
#Load 2019 land cover
LC2019<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/SARLayers/pemmyLC2019updateSAR.tif")

#Make everything that's not Ag 0 
LC2019[]<-ifelse(LC2019[]==2,1,0)


################# IF DOING JUST 2018 -> 2019 ##########################
LC2019[]<-ifelse(LC2019[]==1 & LC2018[]==5,1,0)
#################################################



#################################################



#Import Pemba Vector
Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")

#this loop runs the model for each value in the prior and saves the output in the df created above
for(i in 1:nrow(df)){
  
  
  ###This part does the pixel/pixel validation
  
  ValidationArea<-filter(Pemba,NAME_3==df[i,]$Shehia)
  LC2019_clipped<- crop(LC2019, extent(ValidationArea)) #clip the area
  LC2019_clipped <- mask(LC2019_clipped, ValidationArea) #This deals with NAs
  
  #correct number of conversions
  df[i,]$PixelsConverted2018_2019<-length(which(LC2019_clipped[] ==1))
}


df

