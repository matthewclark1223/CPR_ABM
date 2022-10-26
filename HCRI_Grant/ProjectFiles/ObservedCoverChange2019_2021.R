library(tidyverse)
library(sf)
library(raster)
##2018 to 2019############################################
#Shehia with a considerable amount of coral rag
Pemba <- sf::read_sf("~/Pemba_Project/PembaShapeFile.shp")
studyShehia<-c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
               "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
               "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi")

df<-data.frame(Shehia=studyShehia,PixelsConverted2018_2021=rep(NA,length(studyShehia)))

###Add data for pixel/pixel validation
#Load 2019 observed land cover
LC2021<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/SARLayers/pemmyLC2021updateSAR.tif")
#Load 2018 observed landcover
LC2018<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/SARLayers/pemmyLC2018updateSAR.tif")
#Load 2019 land cover
LC2019<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/SARLayers/pemmyLC2019updateSAR.tif")

#Make everything that's not Ag 0 
LC2021[]<-ifelse(LC2021[]==2,1,0)

#############IF SUMMING CHANGE FROM 2018->2021 AND 2019->2021######################
#filter to just NEW ag converted from coral rag from 2018 ->2019
LC2021_2<-LC2021
LC2021_2[]<-ifelse(LC2021[]==1 & LC2018[]==5,1,0) #Which pixels were CR in 2018 and are Ag in 2021
LC2021[]<-ifelse(LC2021[]==1 & LC2019[]==5,1,LC2021_2[]) # Which pixels were CR in 2019 and are Ag in 2021
##########################################################################



################# IF DOING JUST 2018 -> 2021 ##########################
LC2021[]<-ifelse(LC2021[]==1 & LC2018[]==5,1,0)
#################################################



####  IF SUMMING CHANGE FROM 2019->2021 AND 2018->2019 ######################
LC2021[]<-ifelse(LC2021[]==1 & LC2019[]==5,1,0) #which pixels were CR in 2019 and are ag in 2021
LC2019[]<-ifelse(LC2019[]==2,1,0) #make all ag pixels in 2019 '1' and everything else 0
LC2021[]<-ifelse(LC2019[]==1 & LC2018[]==5,1,LC2021[]) #which pixels were CR in 2018 and were ag in 2019 

##########################################################



#Import Pemba Vector
Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")

#this loop runs the model for each value in the prior and saves the output in the df created above
for(i in 1:nrow(df)){
  
  
  ###This part does the pixel/pixel validation
  
  ValidationArea<-filter(Pemba,NAME_3==df[i,]$Shehia)
  LC2021_clipped<- crop(LC2021, extent(ValidationArea)) #clip the area
  LC2021_clipped <- mask(LC2021_clipped, ValidationArea) #This deals with NAs
  
  #correct number of conversions
  df[i,]$PixelsConverted2018_2021<-length(which(LC2021_clipped[] ==1))
}





df<-data.frame(Shehia=c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
                        "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
                        "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi"),
               PixelsConverted2018_2021=c(961,2223,1139,3448,3579,1503,568,951,1491,910,1203,
                                          2122,1391,1352,943,1484,1302,1650,780))


df2<-data.frame(Shehia=c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
                        "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
                        "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi"),
               PixelsConverted2018_2021=c(614,1385,941,1321,1240,1182,471,726,1124,696,
                                          762,1758,1194,1078,624,988,897,1473,441))



df3<-data.frame(Shehia=c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
                         "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
                         "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi"),
                PixelsConverted2018_2021=c(1431,3717,1535,3663,3899,1914,654,1169,1993,1209,1367,3019,
                                           1839,1859,1102,1602,1480,2336,1051))


################# 2019 to 2021
#Shehia with a considerable amount of coral rag
studyShehia<-c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
               "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
               "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi")

df<-data.frame(Shehia=studyShehia,PixelsConverted2019_2021=rep(NA,length(studyShehia)))

###Add data for pixel/pixel validation
#Load 2019 observed land cover
LC2021<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2021.tif")
#Load 2018 observed landcover
LC2019<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/LandCoverLayers/pemmyLC2019.tif")

#Make everything that's not Ag 0 
LC2021[]<-ifelse(LC2021[]==2,1,0)
#filter to just NEW ag converted from coral rag from 2018 ->2019
LC2021[]<-ifelse(LC2021[]==1 & LC2019[]==5,1,0)
#Import Pemba Vector
Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")

#this loop runs the model for each value in the prior and saves the output in the df created above
for(i in 1:nrow(df)){
  
  
  ###This part does the pixel/pixel validation
  
  ValidationArea<-filter(Pemba,NAME_3==df[i,]$Shehia)
  LC2021_clipped<- crop(LC2021, extent(ValidationArea)) #clip the area
  LC2021_clipped <- mask(LC2021_clipped, ValidationArea) #This deals with NAs
  
  #correct number of conversions
  df[i,]$PixelsConverted2019_2021<-length(which(LC2021_clipped[] ==1))
}





df<-data.frame(Shehia=c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
                        "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
                        "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi"),
               PixelsConverted2019_2021=c(801,1459,709,3143,3268,737,217,557,
                                          786,413,732,955,543,548,555,1178,853,808,579))



