library(raster)
library(sf)
library(tidyverse)


LU_AMB<-function(
  YearsPast2018 = 3, #years (timesteps) to run model
  Wards = NULL,  #character vector or wards to model. Default is full model
  FallowTime = 3, #time (in years) it takes for fallow land to recharge
  AgLimit = 2 #Time (in years) land can be farmed in-between fallow periods
){
  
  Years<-(1:YearsPast2018)+2018
  
  #specifics to this dataset
  stackRS<-raster::stack("PembaFiresAndPredictors.tif")
  names(stackRS)<-c("roads_proximity","slope","soil_cat","fires2019")
  rProbBurn<-raster::raster("PredFire2019.tif")
  
  #These will be used to assign working/fallow ag randomly
  FallowRechargeTime<-FallowTime
  AgLimit<-AgLimit
  propstrt<-AgLimit/FallowRechargeTime
  
  #Import 2018 Lc Data
  Pem2018LC<-raster::raster("~/Pemba_Project/HCRI_Grant/ProjectFiles/Pem_2018_LC_20m_V2.tif")
  
  rLndCvr2018<-Pem2018LC
  rLndCvr2018[which(rLndCvr2018[]==2)]<-sample(c(100,2),size=length(which(rLndCvr2018[]==2)),
                                               replace=T,prob=c(propstrt,1-propstrt))
  rLndCvr2018[]<-as.factor(ifelse(rLndCvr2018[] %in% c(1,5),"Burnable",
                                  ifelse(rLndCvr2018[] == 2,"Agriculture",
                                         ifelse(rLndCvr2018[] == 100,"Fallow","Unburnable")))) 
  
  
  #stack all starting layers together
  rstack<-stack(rProbBurn,rLndCvr2018 )
  names(rstack)<-c("ProbBurn","LndCvr2018") #set the names
  rstack$LndCvr2018[]<- as.integer(rstack$LndCvr2018)[] #this should be an integer. 1==ag,2==burnable, 3==fallow, 4==unburnable
  
  ######CROP for trialing
  Pemba <- read_sf("~/Pemba_Project/PembaShapeFile.shp")
  
  if(!is.null(Wards)){ #If wards isn't null, filter by selected wards
    PembaSUB <- Pemba%>%filter(NAME_3 %in% Wards)
  }
  if(is.null(Wards)){ #If wards is null, do the whole island
    PembaSUB <- Pemba
  }
  
  #PembaSUB <- Pemba%>%filter(NAME_3 %in% c("Makangale"))
  #ggplot() + geom_sf(data = PembaSUB)+ geom_sf_label(data=PembaSUB,aes(label=NAME_3),size=3)+ theme_bw()
  r2 <- crop(rstack, extent(PembaSUB))
  rstack <- mask(r2, PembaSUB)
  
  
  
  ############### model copied from simple abm
  ag2018<-which(rstack$LndCvr2018[]==1) #ag from 2018 land cover layer
  fallow2018<-which(rstack$LndCvr2018[]==3) #fallow/bare from 2018 land cover layer
  burnable2018<-which(rstack$LndCvr2018[]==2) #burnable from 2018 landcover layer
  unburnable2018<-which(rstack$LndCvr2018[]==4) #unburnable from 2018 land cover layer
  rstack$ag2018<-NA #Make all NA to start
  rstack$ag2018[ag2018]<-sample(1:AgLimit,size=length(ag2018),replace=T) #plots that were already ag in 2018 have been at for 2 years

  
  #Make starting fallow layer
  rstack$fallow2018<-NA #Make all NA to start
  rstack$fallow2018[fallow2018]<-sample(1:FallowRechargeTime,size=length(fallow2018),replace=T) 
  
  #Make starting burnable layer
  rstack$burnable2018<-NA #make allNA to start
  rstack$burnable2018[burnable2018]<-1 #make 1 if burnable
  
  #Make starting UNburnable layer
  rstack$Unburnable2018<-NA
  rstack$Unburnable2018[unburnable2018]<-1
  
 NewLayersNum<-4*YearsPast2018 #4 here is ag, fallow, unurnable, & burnable
 for(l in 1:NewLayersNum){
   rstack[[6+l]]<-NA
   names(rstack[[6]])<-"Unburnable2018"
   
 }
 
 newlayernames<-c(paste0("ag",Years),paste0("fallow",Years),paste0("burnable",Years),paste0("Unburnable",Years))
 names(rstack)[7:(6+NewLayersNum)]<- newlayernames 
 
 #make a container vector for the  burn predictions
 rstack$NEWBurn<-0 #create a 0's raster with same extent as existing for new conversions at each time step
 
  for(y in 1:length(Years)){
    
    names(rstack)[7:(6+NewLayersNum)]<- newlayernames #Not sure why these keep changing
    x<-Years[y]-1
    aglayer<-rstack[[match(paste0("ag",x),names(rstack))]]
    fallowlayer<-rstack[[match(paste0("fallow",x),names(rstack))]]
    burnablelayer<-rstack[[match(paste0("burnable",x),names(rstack))]]
    unburnablelayer<-rstack[[match(paste0("Unburnable",x),names(rstack))]]
    
    
    rstack$NEWBurn[]<-0
    
  
  NewFallow<-which(aglayer[]==2)#which ones are going fallow now?
  
  for(i in 1:length(NewFallow)){
    
    neighborCells<-adjacent(rstack, cells=NewFallow[i], directions=8, pairs=TRUE)[,2] #Cells neighboring each new fallow cell
    
    #subtract cells already burned this timestep
    if(length(which(neighborCells %in% which(rstack$NEWBurn[]==1)))>0){#only subtract if there's something to subtract!
      neighborCells<-neighborCells[-which(neighborCells %in% which(rstack$NEWBurn[]==1))] }
    
    #subtract out unburnable
    if(length(which(neighborCells %in% which(unburnablelayer[]==1)))>0){#only subtract if there's something to subtract!
      neighborCells<-neighborCells[-which(neighborCells %in% which(unburnablelayer[]==1))] }
    
    #subtract our ag already
    if(length(which(neighborCells %in% which(aglayer[]>=1)))>0){ #only subtract if there's something to subtract!
      neighborCells<-neighborCells[-which(neighborCells %in% which(aglayer[]>=1))] }
    
    #subtract out fallow under fallow recharge time
    if(length(which(neighborCells %in% which(fallowlayer[]<FallowRechargeTime)))>0){ #only subtract if there's something to subtract!
      neighborCells<-neighborCells[-which(neighborCells %in% which(fallowlayer[]<FallowRechargeTime))] }
    
    #If there's no plots nearby at the fallow limit, then we have to chop some trees if theyre around
    
    if(length(which(neighborCells %in% which(fallowlayer[]==FallowRechargeTime)))==0 & #if there's no fallowland nearby
       length(which(neighborCells %in% which(burnablelayer[]==1)))>0 ){ #and there IS burnable land
      #which one looks the best for burning
      mostlikelyBurn<-neighborCells[which(rstack$ProbBurn[neighborCells] == max(rstack$ProbBurn[neighborCells]))]
      rstack$NEWBurn[mostlikelyBurn]<-1 #burn it
    }
    
    #add something here about moving somewhere new if there's nothing fallowed out or burnable around. Maybe?
    
    #Show progress
    print((i/length(NewFallow))*100)
    
    
  }
  
  #convert new burn to ag and advance ag/fallow cycles
  z<-Years[y]
  #rstack[[match(paste0("fallow",z),names(rstack))]][]<-fallowlayer[]
  #rstack[[match(paste0("ag",z),names(rstack))]][]<-aglayer[]
  
  rstack[[match(paste0("burnable",z),names(rstack))]][]<-burnablelayer[] #Copy over last year's burnable veg
  rstack[[match(paste0("burnable",z),names(rstack))]][which(rstack$NEWBurn[]==1)]<-NA #Remove the ones that we predict burned
  rstack[[match(paste0("ag",z),names(rstack))]][which(rstack$NEWBurn[]==1)]<-1L #make those ones into 1st year agriculture
  rstack[[match(paste0("ag",z),names(rstack))]][which(aglayer[]==1)]<-2L #ones that were 1st year ag last year are now 2nd year ag
  rstack[[match(paste0("ag",z),names(rstack))]][which(fallowlayer[]==FallowRechargeTime)]<-1L #Ones that were at the end of the fallow cycle last year are ag now
  
  
  rstack[[match(paste0("Unburnable",z),names(rstack))]][]<-unburnablelayer[]
  
  rstack[[match(paste0("fallow",z),names(rstack))]][which(aglayer[]==AgLimit)]<-1L
  
  
  
  
  #rstack[[match(paste0("fallow",z),names(rstack))]][which(fallowlayer[]==3)]<-NA  
  
  
  rstack[[match(paste0("fallow",z),names(rstack))]][which(fallowlayer[]==2)]<-3L
  rstack[[match(paste0("fallow",z),names(rstack))]][which(fallowlayer[]==1)]<-2L
  
  
  rstack[[match(paste0("fallow",z),names(rstack))]][which(fallowlayer[]==FallowRechargeTime)]<-NA
  rstack[[match(paste0("ag",z),names(rstack))]][which(aglayer[]==AgLimit)]<-NA
  
  
  }
  
  
  }
  
  #BUILD this into the model...too tired to do it now
  rstack$LndCvr2019<-NA
  rstack$LndCvr2019[]<-ifelse(!is.na(rstack$ag2019[]),1,
                              ifelse(!is.na(rstack$burnable2019)[],2,
                                     ifelse(!is.na(rstack$fallow2019[]),3,
                                            ifelse(!is.na(rstack$Unburnable2019[]),4,NA))))
  
  rstack$LndCvr2020<-NA
  rstack$LndCvr2020[]<-ifelse(!is.na(rstack$ag2020[]),1,
                              ifelse(!is.na(rstack$burnable2020)[],2,
                                     ifelse(!is.na(rstack$fallow2020[]),3,
                                            ifelse(!is.na(rstack$Unburnable2020[]),4,NA))))
  
  rstack$LndCvr2021<-NA
  rstack$LndCvr2021[]<-ifelse(!is.na(rstack$ag2021[]),1,
                              ifelse(!is.na(rstack$burnable2021)[],2,
                                     ifelse(!is.na(rstack$fallow2021[]),3,
                                            ifelse(!is.na(rstack$Unburnable2021[]),4,NA))))
  
  
 plot(rstack$LndCvr2019)
  hist(rstack$ag2021) # Ag is not repopulating!!!
 
  ###
  plotfun<-function(x){
  r <- x
  r[]<-as.factor(r[])
  rat <- levels(r)[[1]]
  rat[["landcover"]] <- c("Productive","Fallow","Convertable", "Non-convertable")
  levels(r) <- rat
  
  ## Plot
  cols<-c("#ffd966","#b6d7a8ff","#7f6000","#969696")
  cols<-c("#7f6000","#b6d7a8ff","#7f6000","#969696") #keep if all rotational ag should be the same color
  rasterVis::levelplot(r, col.regions=cols, xlab="", ylab="")}
  
  plotfun(rstack$LndCvr2018)
  plotfun(rstack$LndCvr2019)
  plotfun(rstack$LndCvr2020)
  plotfun(rstack$LndCvr2021)
  
  x<-stack(rstack$LndCvr2018,rstack$LndCvr2019,rstack$LndCvr2020,rstack$LndCvr2021)
  animate(x,col.regions=cols, xlab="", ylab="")
  ###


LU_AMB(YearsPast2018 = 2, #years (timesteps) to run model
       Wards = c("Makangale","Msuka Magharibi","Msuka Mashariki"),  #character vector or wards to model. Default is full model
       FallowTime = 3, #time (in years) it takes for fallow land to recharge
       AgLimit = 2
  
)
names(rstack)
raster::plot(rstack$burnable2019)
raster::plot(rstack$burnable2020)
raster::plot(rstack$burnable2021)
