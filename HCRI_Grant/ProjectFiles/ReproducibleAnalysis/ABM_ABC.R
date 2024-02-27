#Code to run the Approximate Bayesian Computation to figure out
#What is the most likely "intrinsic growth" parameter for the 
#expansion of agricultural land into coral rag forests in Pemba Island Tanzania


#The goal here is to take some data where we know the outcome, in this case
#the land cover from Pemba in 2019

#And use a generative model of how that data might have been produced (the abm), 
#To figure out what a likely parameter value in the model was that produced those data

#We also need a prior on what the value(s) we are trying to estimate are

#Lastly, we need a criterion for when the simulated data "matches" the actual data
#We will likely not often get an exact match, so we need some other metric


#First let's make the prior
library(raster)
library(sf)
library(tidyverse)
set.seed(1)


prior<-rgamma(2000,5.5,0.8)  #Prior on the rate of intrinsic growth of ag land
#hist(prior)

#Shehia with a considerable amount of coral rag
studyShehia<-c("Fundo","Kangagani","Kojani","Shumba Mjini","Maziwa Ng'ombe","Mjini Ole","Ole",
               "Uwandani","Vitongoji","Kibokoni","Mvumoni","Pujini","Dodo","Chambani",
               "Jombwe","Shamiani","Muambe","Kiwani","Mjini Wingwi")

##### For Testing use these #############
## Comment out if not testing ####
priorTEST<-rgamma(5,5.5,0.8) 

studyShehiaTEST<-c("Fundo","Kangagani")
abc_data<-expand.grid(studyShehiaTEST,priorTEST)

#################### End of testing ##################################


#Create data for each run
abc_data<-expand.grid(studyShehia,prior)
names(abc_data)[1:2]<-c("Shehia","GrowthRate")

#Create an empty column to fill with the estimated number of conversion events 
abc_data$numberConversions<-rep(NA,nrow(abc_data))
abc_data$observedConversions<-rep(NA,nrow(abc_data))
abc_data$numberCorrectPixelConversions=rep(NA,length(prior))
abc_data$percentCorrectPixelConversions=rep(NA,length(prior))

#create the LU_abm function
source("./Full_LandUse_ABM.R")


###Add data for pixel/pixel validation
#Load 2019 observed land cover
LC2019<-raster::raster("./pemmyLC2019updateSAR.tif")
#Load 2018 observed landcover
LC2018<-raster::raster("./pemmyLC2018updateSAR.tif")
#Make everything that's not Ag 0 
LC2019[]<-ifelse(LC2019[]==2,1,0)
#filter to just NEW ag converted from coral rag from 2018 ->2019
LC2019[]<-ifelse(LC2019[]==1 & LC2018[]==5,1,0)
#Import Pemba Vector
Pemba <- read_sf("./PembaShapeFile.shp")

#this loop runs the model for each value in the prior and saves the output in the df created above
for(i in 1:nrow(abc_data)){
  LU_AMB(YearsPast2018 = 1, #years (timesteps) to run model
         Wards = abc_data[i,]$Shehia,  #character vector or wards to model. Default is full model
         FallowTime = 3, #time (in years) it takes for fallow land to recharge
         AgLimit = 2, #ag time. These numbers come from the farmer interviews
         IntrinsicExp = abc_data[i,]$GrowthRate
   )
  
  abc_data[i,]$numberConversions<-sum(rstack$NEWBurn[])  #this part adds the result to the dataframe
  
  ###This part does the pixel/pixel validation
  
  ValidationArea<-filter(Pemba,NAME_3==abc_data[i,]$Shehia)
  LC2019_clipped<- crop(LC2019, extent(ValidationArea)) #clip the area
  LC2019_clipped <- mask(LC2019_clipped, ValidationArea) #This deals with NAs
  
  #correct number of conversions
  abc_data[i,]$observedConversions<-length(which(LC2019_clipped[] ==1))
  
  #Correct exact match pixel conversions
  abc_data[i,]$numberCorrectPixelConversions<-length(which(rstack$NEWBurn[]== 1 & 
                                                             LC2019_clipped[] ==1))  
  
  #add the percent of pixel conversions that are exactly correct. 
  abc_data[i,]$percentCorrectPixelConversions<-abc_data[i,]$numberCorrectPixelConversions/length(which(rstack$NEWBurn[]== 1))
  
  }



###

