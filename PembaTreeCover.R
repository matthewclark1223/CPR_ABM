library("raster")
library("rgeos")
library("rgdal")
library(sf)
library(magrittr)
library("rasterVis")
#library(viridis)


x<-raster("Hansen_GFC-2019-v1.7_treecover2000_00N_030E.tif") #tree cover data from https://data.globalforestwatch.org/datasets/tree-cover-2000
#plot(x) this takes awhile to run and is worthless for this demonstration besides making sure it worked

Tanzania<-read_sf("~/Pemba_Project/MapData/TZA_adm0.shp") #tanzania jurisdictional boundaries
Pemba <- st_crop(Tanzania, xmin = 39.35, xmax = 40.0,
                 ymin = -5.75, ymax = -4.85)  #pemba only

z<-crop(x,Pemba)#crop to pemba
z[z==0]<-NA #make water NA
plot(z) #make sure this worked for the initial map


model <- function(r, Time) { #this is the very simple model 
  outputs <- vector("list", Time) #list of length 'time'
  outputs[[1]]<-r #the first object in the list is the initialization raster
  for(t in 2:Time) { 
    
    r_old<-outputs[[t-1]] #raster from last time step is now the thing getting things done to it
     new_cov<-r_old-(0.1*r_old) # minus a deforestation rate of 3%
     new_cov[z<0]<-0 #don't let % cover go below 0
     outputs[[t]] <-new_cov
      
  }
  return(outputs) #return rasters as list
}

Mod <- model(r = z, Time = 5) #run the model, create 5 rasters


### 3 #################################################
####plot the output

#plot(stack(Mod) ) #this is a simpler, but worse option that the code below

levelplot(stack(Mod) )  #make a raster stack and plot

source("MakePembaVector.R") #load the vector data with the deforestation rates from Collins et al 2021

#make a raster with the values for deforestation for each pixel so we can multiply times the 
#2000 tree cover

def2001_rast<-raster()
#extent(def2001_rast)<-extent(Pemba_vector)  dont run
extent(def2001_rast)<-extent(z) 
  
res(def2001_rast)<-res(z) #set the pixel size to be the same

Pem_M<-st_cast(Pemba_vector,"MULTIPOLYGON")
def1<-fasterize::fasterize(Pem_M,def2001_rast,field="Annual_rate_of_change_2001_2010" )
def2<-fasterize::fasterize(Pem_M,def2001_rast,field="Annual_rate_of_change_2010_2018" )

plot(def2)

vec_rast<-list(def1,def2)
plot(stack(vec_rast))

plot(def2)
plot(z)

plot(z*(def2*0.01))

#now run the look
Cov_model <- function(r, Time) { #this is the very simple model 
  outputs <- vector("list", Time) #list of length 'time'
  outputs[[1]]<-r #the first object in the list is the initialization raster
  for(t in 2:11) { #2001 - 2010
    
    r_old<-outputs[[t-1]] #raster from last time step is now the thing getting things done to it
    new_cov<-r_old+(r_old*(def1*0.01)) # deforestation rates before 2010 from collins 2021 
    p<-new_cov>100
    new_cov[p]<-100 #dont let % cover go above 100
    
    p<-new_cov<0
    new_cov[p]<-0 #dont let % cover go below 0
    outputs[[t]] <-new_cov}
    
    
    for(t in 12:19) { #2011 - 2019
      
      r_old<-outputs[[t-1]] #raster from last time step is now the thing getting things done to it
      new_cov<-r_old+(r_old*(def2*0.01)) #deforestation rates after 2010 from collins 2021 
      p<-new_cov>100
      new_cov[p]<-100 #dont let % cover go above 100
      
      p<-new_cov<0
      new_cov[p]<-0 #dont let % cover go below 0
      outputs[[t]] <-new_cov
    
  }
  return(outputs) #return rasters as list
}

Mod <- Cov_model(r = z, Time = 19) #run the model, create rasters for each year. Probably dont need 18 as input since it's fixed


### 3 #################################################
####plot the output

#plot(stack(dumMod) ) #this is a simpler, but worse option that the code below

da<-stack(Mod)
names(da)<-paste0("Year.",as.character(2000:2018))
#levelplot(da)
animate(da)

animation::saveGIF(animate(da),filename="TreeCover.gif" )





