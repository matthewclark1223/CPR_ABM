library(raster)
library(sf)
library(tidyverse)
#specifics to this dataset
setwd()
stackRS<-raster::stack("./ProjectFiles/PembaFiresAndPredictors.tif")
names(stackRS)<-c("roads_proximity","slope","soil_cat","fires2019")
rProbBurn<-raster::raster("./ProjectFiles/PredFire2019.tif")

#These will be used to assign working/fallow ag randomly
FallowRechargeTime<-3
AgLimit<-2
propstrt<-AgLimit/FallowRechargeTime

#Import 2018 Lc Data
Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/pemmyRF2018_R3.tif")

rLndCvr2018<-Pem2018LC
rLndCvr2018[which(rLndCvr2018[]==2)]<-sample(c(100,2),size=length(which(rLndCvr2018[]==2)), #assign 100 to Fallow 
                                             replace=T,prob=c(propstrt,1-propstrt)) #2 is productive ag
#0 = Mangrove
#1 = HF
#2 = agriculture
#3 = Urban
#4 = Bare
#5 = Coral rag
#6 = OWV/agroforestry
#7 = water
### ONLY CORAL RAG BURNABLE
rLndCvr2018[]<-as.factor(ifelse(rLndCvr2018[] %in% c(0,1,3,4,6,7),"Unburnable", #everything besides ag, fallow, and CR
                                ifelse(rLndCvr2018[] == 2,"Agriculture", #productive ag
                                       ifelse(rLndCvr2018[] == 100,"Fallow","Burnable")))) #Fallow and CR


#stack all starting layers together
rstack<-stack(stackRS$fires2019,rProbBurn,rLndCvr2018 )
names(rstack)<-c("Burn2019","ProbBurn","LndCvr2018") #set the names
rstack$LndCvr2018<- as.integer(rstack$LndCvr2018) #this should be an integer

######CROP for trialing
Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")
#PembaSUB <- Pemba%>%filter(NAME_3 %in% c("Makangale","Msuka Magharibi","Msuka Mashariki"))
PembaSUB <- Pemba%>%dplyr::filter(NAME_3=="Kangagani")
#ggplot() + geom_sf(data = PembaSUB)+ geom_sf_label(data=PembaSUB,aes(label=NAME_3),size=3)+ theme_bw()
r2 <- crop(rstack, extent(PembaSUB))
rstack <- mask(r2, PembaSUB)



############### model copied from simple abm
ag2018<-which(rstack$LndCvr2018[]==1) #ag from 2018 land cover layer
#burned2019<-which(rstack$Burn2019[]==1) #get cell numbers that burned in 2019
fallow2018<-which(rstack$LndCvr2018[]==2) #fallow/bare from 2018 land cover layer
burnable2018<-which(rstack$LndCvr2018[]==3) #burnable from 2018 landcover layer
unburnable2018<-which(rstack$LndCvr2018[]==4) #unburnable from 2018 land cover layer



#Make the starting ag layer
rstack$ag2019<-NA #Make all NA to start
rstack$ag2019[ag2018]<-sample(2:AgLimit,size=length(ag2018),replace=T) #plots that were already ag in 2018 have been at for 2 years
rstack$ag2019[burned2019]<-1

#Make starting fallow layer
rstack$fallow2019<-NA #Make all NA to start
rstack$fallow2019[fallow2018]<-sample(2:FallowRechargeTime,size=length(fallow2018),replace=T)

#Make starting burnable layer
burnable2019<-burnable2018[!burnable2018 %in% burned2019] #remove the ones that already burned
rstack$burnable2019<-NA #make allNA to start
rstack$burnable2019[burnable2019]<-1 #make 1 if burnable

#Make starting UNburnable layer
rstack$Unburnable2019<-NA
rstack$Unburnable2019[unburnable2018]<-1


#make a container vector for the 2020 burn predictions
rstack$predBurn2020<-0 #create a 0's raster with same extent as existing


NewFallow<-which(rstack$ag2019[]==2)#which ones are going fallow now?


for(i in 1:length(NewFallow)){
  
  neighborCells<-adjacent(rstack, cells=NewFallow[i], directions=8, pairs=TRUE)[,2] #Cells neighboring each new fallow cell
  
  #subtract cells already burned this timestep
  if(length(which(neighborCells %in% which(rstack$predBurn2020[]==1)))>0){#only subtract if there's something to subtract!
    neighborCells<-neighborCells[-which(neighborCells %in% which(rstack$predBurn2020[]==1))] }
  
  #subtract out unburnable
  if(length(which(neighborCells %in% which(rstack$Unburnable2019[]==1)))>0){#only subtract if there's something to subtract!
    neighborCells<-neighborCells[-which(neighborCells %in% which(rstack$Unburnable2019[]==1))] }
  
  #subtract our ag already
  if(length(which(neighborCells %in% which(rstack$ag2019[]>=1)))>0){ #only subtract if there's something to subtract!
    neighborCells<-neighborCells[-which(neighborCells %in% which(rstack$ag2019[]>=1))] }
  
  #subtract out fallow under fallow recharge time
  if(length(which(neighborCells %in% which(rstack$fallow2019[]<FallowRechargeTime)))>0){ #only subtract if there's something to subtract!
    neighborCells<-neighborCells[-which(neighborCells %in% which(rstack$fallow2019[]<FallowRechargeTime))] }
  
  #If there's no plots nearby at the fallow limit, then we have to chop some trees if theyre around
  
  if(length(which(neighborCells %in% which(rstack$fallow2019[]==FallowRechargeTime)))==0 & #if there's no fallowland nearby
     length(which(neighborCells %in% which(rstack$burnable2019[]==1)))>0 ){ #and there IS burnable land
    #which one looks the best for burning
    mostlikelyBurn<-neighborCells[which(rstack$ProbBurn[neighborCells] == max(rstack$ProbBurn[neighborCells]))]
    rstack$predBurn2020[mostlikelyBurn]<-1 #burn it
  }
  
  #add something here about moving somewhere new if there's nothing fallowed out or burnable around. Maybe?
  
  #Show progress
  print((i/length(NewFallow))*100)
  
  
}



length(NewFallow)
length(which(rstack$predBurn2020[]==1))


plot(rstack$predBurn2020)

rstackNAs<-rstack

rstackNAs[rstackNAs<=0]<-NA
r_df<-raster::as.data.frame(rstackNAs, xy = TRUE) 

ggplot(data=r_df,aes(x=x,y=y))+
  geom_tile(data = na.omit(r_df%>%dplyr::select(Burn2019,x,y)) , 
            aes(x = x, y = y),fill="red") +
  geom_tile(data = na.omit(r_df%>%dplyr::select(predBurn2020,x,y)) , 
            aes(x = x, y = y),fill="blue") +
  
  theme_bw()


ggplot(data=r_df,aes(x=x,y=y))+
  geom_tile(data = na.omit(r_df%>%dplyr::select(LndCvr2018,x,y)) , 
            aes(x = x, y = y,fill=as.character(LndCvr2018))) +
  #geom_tile(data = na.omit(r_df%>%dplyr::select(predBurn2020,x,y)) , 
   #         aes(x = x, y = y),fill="yellow") +
  
  theme_bw()

####plot with rasterVis
r <- as.factor(rstack$LndCvr2018)
rat <- levels(r)[[1]]
rat[["landcover"]] <- c("Productive","Fallow","Convertable", "Non-convertable")
levels(r) <- rat

## Plot
cols<-c("#ffd966","#7f6000","#b6d7a8ff","#969696")
rasterVis::levelplot(r, col.regions=cols, xlab="", ylab="")

