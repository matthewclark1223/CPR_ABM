library(raster)
library(sf)
library(tidyverse)



LU_AMB<-function(
  YearsPast2018 = 3, #years (timesteps) to run model
  Wards = "Kojani",  #character vector or wards to model. Default is full model
  FallowTime = 3, #time (in years) it takes for fallow land to recharge
  AgLimit = 2, #Time (in years) land can be farmed in-between fallow periods
  IntrinsicExp = 3 #Intrinsic rate (percentage) of agricultural expansion into the CR forest (pop growth, market integration, etc)
){
  
  Years<-(1:YearsPast2018)+2018
  
  #specifics to this dataset
  stackRS<-raster::stack("./PembaFiresAndPredictors.tif")
  names(stackRS)<-c("roads_proximity","soil_cat")
  #rProbBurn<-raster::raster("./ProjectFiles/PredFire2019.tif")
  
  #These will be used to assign working/fallow ag randomly
  FallowRechargeTime<-FallowTime
  AgLimit<-AgLimit
  
  propstrt<-FallowRechargeTime/(AgLimit+FallowRechargeTime) #probability that any ag pixel is fallow at the start
  
  IntrinsicExp=IntrinsicExp/100 # get this on a scale we can multiply by
  
  
  #Import 2018 Lc Data
  Pem2018LC<-raster::raster("./pemmyLC2018updateSAR.tif")
  
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
  
  #changed below for a diatance to road layer instead of prob of fire layer
  #rstack<-stack(rProbBurn,rLndCvr2018 )
  #names(rstack)<-c("ProbBurn","LndCvr2018") #set the names
  
  rstack<-stack(stackRS$roads_proximity,rLndCvr2018 )
  names(rstack)<-c("roads_proximity","LndCvr2018") #set the names
  rstack$LndCvr2018[]<- as.integer(rstack$LndCvr2018)[] #this should be an integer. 1==ag,2==burnable, 3==fallow, 4==unburnable
  
  ######CROP for trialing
  Pemba <- read_sf("./PembaShapeFile.shp")
  
  if(!is.null(Wards)){ #If wards isn't null, filter by selected wards
    PembaSUB <- Pemba%>%filter(NAME_3 %in% Wards)
  }
  if(is.null(Wards)){ #If wards is null, do the whole island
    PembaSUB <- Pemba
  }
  
  
  r2 <- crop(rstack, extent(PembaSUB))
  rstack <- mask(r2, PembaSUB)
  
  
  
  ############### model copied from simple abm
  ag2018<-which(rstack$LndCvr2018[]==1) #ag from 2018 land cover layer
  fallow2018<-which(rstack$LndCvr2018[]==3) #fallow/bare from 2018 land cover layer
  burnable2018<-which(rstack$LndCvr2018[]==2) #burnable from 2018 landcover layer
  unburnable2018<-which(rstack$LndCvr2018[]==4) #unburnable from 2018 land cover layer
  rstack$ag2018<-NA #Make all NA to start
  rstack$ag2018[ag2018]<-sample(1:AgLimit,size=length(ag2018),replace=T) #equally assign all possible ag years
  
  
  #Make starting fallow layer
  rstack$fallow2018<-NA #Make all NA to start
  rstack$fallow2018[fallow2018]<-sample(1:FallowRechargeTime,size=length(fallow2018),replace=T) #equally assign all possible fallow years
  
  #Make starting burnable layer
  rstack$burnable2018<-NA #make allNA to start
  rstack$burnable2018[burnable2018]<-1 #make 1 if burnable
  
  #Make starting UNburnable layer
  rstack$Unburnable2018<-NA
  rstack$Unburnable2018[unburnable2018]<-1
  
  NewLayersNum<-4*YearsPast2018 #4 here is ag, fallow, unurnable
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
    
    
    NewFallow<-which(aglayer[]==AgLimit)#which ones are going fallow now?
    
    for(i in 1:length(NewFallow)){
      
      neighborCells<-adjacent(rstack, cells=NewFallow[i], directions=8, pairs=TRUE)[,2] #Cells neighboring each new fallow cell
      
      #subtract cells already burned this timestep
      if(length(which(neighborCells %in% which(rstack$NEWBurn[]==1)))>0){#only subtract if there's something to subtract!
        neighborCells<-neighborCells[-which(neighborCells %in% which(rstack$NEWBurn[]==1))] }
      
      #subtract out unburnable
      if(length(which(neighborCells %in% which(unburnablelayer[]==1)))>0){#only subtract if there's something to subtract!
        neighborCells<-neighborCells[-which(neighborCells %in% which(unburnablelayer[]==1))] }
      
      #subtract out ag already
      if(length(which(neighborCells %in% which(aglayer[]>=1)))>0){ #only subtract if there's something to subtract!
        neighborCells<-neighborCells[-which(neighborCells %in% which(aglayer[]>=1))] }
      
      #subtract out fallow under fallow recharge time
      if(length(which(neighborCells %in% which(fallowlayer[]<FallowRechargeTime)))>0){ #only subtract if there's something to subtract!
        neighborCells<-neighborCells[-which(neighborCells %in% which(fallowlayer[]<FallowRechargeTime))] }
      
      #If there's no plots nearby at the fallow limit, then we have to chop some trees if theyre around
      
      if(length(which(neighborCells %in% which(fallowlayer[]==FallowRechargeTime)))==0 & #if there's no fallowland nearby
         length(which(neighborCells %in% which(burnablelayer[]==1)))>0 ){ #and there IS burnable land
        #which one looks the best for burning
        #Replaced probability layer here with road proximity
        #Substitute for distance to road layer based on small roads!
        #mostlikelyBurn<-neighborCells[which(rstack$ProbBurn[neighborCells] == max(rstack$ProbBurn[neighborCells]))]
        #rstack$NEWBurn[mostlikelyBurn]<-1 #burn it
        
        #remove below if using a probability of fire layer instead of distance to road!
        #just having distance to roads makes it "streaky".
        #Need to have some way to first burn spots touching Ag land, then choose which is closest to the road. 
        NC<-data.frame(Cell=rep(NA,length(neighborCells)),NumAg=rep(NA,length(neighborCells)))  #make df of neighbr cells
        for(nc in 1:length(neighborCells)){ #for each of them
          adj<-adjacent(rstack, cells=neighborCells[nc], directions=8, pairs=TRUE)[,2] ##what are the cells around it
          #how many of those are ag in 2018. Should probably UPDATE to be most recent ag year!!
          agSurr<-length(which(is.na(rstack[[3]][adj])==F)) 
          NC[nc,1]<-neighborCells[nc] #keep naighbor cell number
          NC[nc,2]<-agSurr} #add in the number of surroundign Ag pixels
        NC<-NC[NC$NumAg==max(NC$NumAg,na.rm=T),] #only keep the ones that are equal to the max
        
        if(nrow(NC)>=2 & is.integer(NC$NumAg)==TRUE){
          neighborCells<-NC$Cell} #reassign neighbor cells
        
        #Now choose which is closest to the road
        mostlikelyBurn<-neighborCells[which(rstack$roads_proximity[neighborCells] == min(rstack$roads_proximity[neighborCells],na.rm=T))]
        rstack$NEWBurn[mostlikelyBurn]<-1 #burn it
        
        
        
      }
      
      #add something here about moving somewhere new if there's nothing fallowed out or burnable around. Maybe?
      
      #Show progress
      print((i/length(NewFallow))*100)
      
      
    }
    
    
    if(IntrinsicExp>0){
      ###Now add in the intrinsic growth rate
      numIntGrwth<-round(length(which(is.na(burnablelayer[])==FALSE))*IntrinsicExp) #how many should be converted
      availIntGrwth<-which(rstack$NEWBurn[]==0 & is.na(burnablelayer[])==FALSE) #which ones are available 
      availRDS<-data.frame(pix=availIntGrwth,val=rstack$roads_proximity[availIntGrwth],NBC=rep(NA,length(availIntGrwth))) # add these to the road distance values
      
      #get the number of ag neighbor cells for each one of these CR pixels THIS IS TOO SLOW
      #This for loop has been replaced by the vectorized code with the xx below
      #for(i in 1:nrow(availRDS)){
       # cellz<-adjacent(aglayer, cells=availRDS[i ,]$pix, directions=8, pairs=TRUE)[,2]
        #availRDS[i,]$NBC<-length(which(is.na(aglayer[cellz])==FALSE))
    #  }
      
      ###
      xx<-as.data.frame(adjacent(aglayer, cells=availRDS$pix, directions=8, pairs=TRUE))
        
      xx<-xx%>%filter(to %in%which(is.na(aglayer[])==FALSE))%>%
        group_by(from)%>%count()%>%mutate(pix=from)
        
      xx<-base::merge(xx,availRDS,by="pix",all.y=TRUE)
      xx[which(is.na(xx$n)==TRUE),]$n<-0
      availRDS<-xx%>%dplyr::select(pix,n,val)

      availRDS<-dplyr::arrange(availRDS,desc(n),val ) #arrange first by the number of neighboring ag cells
      #then by the road prox
      
      
      IntGrwthConvert<-availRDS[1:numIntGrwth,]$pix #vector of the pixels to convert
      rstack$NEWBurn[IntGrwthConvert]<-1 #convert them
      ### end section of intrinsic growth
    }
    
    #convert new burn to ag and advance ag/fallow cycles
    z<-Years[y]
    #rstack[[match(paste0("fallow",z),names(rstack))]][]<-fallowlayer[]
    #rstack[[match(paste0("ag",z),names(rstack))]][]<-aglayer[]
    
    rstack[[match(paste0("burnable",z),names(rstack))]][]<-burnablelayer[] #Copy over last year's burnable veg
    rstack[[match(paste0("burnable",z),names(rstack))]][which(rstack$NEWBurn[]==1)]<-NA #Remove the ones that we predict burned
    
    if(AgLimit>=5){rstack[[match(paste0("ag",z),names(rstack))]][which(aglayer[]==4)]<-5L}
    if(AgLimit>=4){rstack[[match(paste0("ag",z),names(rstack))]][which(aglayer[]==3)]<-4L}
    if(AgLimit>=3){rstack[[match(paste0("ag",z),names(rstack))]][which(aglayer[]==2)]<-3L}
    if(AgLimit>=2){rstack[[match(paste0("ag",z),names(rstack))]][which(aglayer[]==1)]<-2L} #Ag that were 1st year ag last year are now 2nd year ag
    rstack[[match(paste0("ag",z),names(rstack))]][which(rstack$NEWBurn[]==1)]<-1L #make those ones into 1st year agriculture
    
    rstack[[match(paste0("ag",z),names(rstack))]][which(fallowlayer[]==FallowRechargeTime)]<-1L #Ones that were at the end of the fallow cycle last year are ag now
    
    
    rstack[[match(paste0("Unburnable",z),names(rstack))]][]<-unburnablelayer[] #unconvertable stays unconvertable
    
    
    
    
    
    
    #rstack[[match(paste0("fallow",z),names(rstack))]][which(fallowlayer[]==3)]<-NA  
    if(FallowRechargeTime>=5){rstack[[match(paste0("fallow",z),names(rstack))]][which(fallowlayer[]==4)]<-5L}#NEW
    if(FallowRechargeTime>=4){rstack[[match(paste0("fallow",z),names(rstack))]][which(fallowlayer[]==3)]<-4L} #NEW
    if(FallowRechargeTime>=3){rstack[[match(paste0("fallow",z),names(rstack))]][which(fallowlayer[]==2)]<-3L} #2nd year fallow becomes 3rd year
    if(FallowRechargeTime>=2){rstack[[match(paste0("fallow",z),names(rstack))]][which(fallowlayer[]==1)]<-2L}
    rstack[[match(paste0("fallow",z),names(rstack))]][which(aglayer[]==AgLimit)]<-1L #if they are at the end of the Ag limit, then they become fallow
    
    rstack[[match(paste0("fallow",z),names(rstack))]][which(fallowlayer[]==FallowRechargeTime)]<-NA
    rstack[[match(paste0("ag",z),names(rstack))]][which(aglayer[]==AgLimit)]<-NA
    
    
  }
  
  outcomeLyrs<-length(Years) 
  Lyrs<-length(names(rstack))
  for(l in 1:outcomeLyrs){
    rstack[[Lyrs+l]]<-NA
    
  }
  
  newLyrnames<-c(paste0("LndCvr",Years))
  names(rstack)[(Lyrs+1):(Lyrs+outcomeLyrs)]<- newLyrnames 
  
  
  
  for(yr in 1:length(Years)){
    ag<-which(names(rstack)==paste0("ag",Years[yr]))
    brnable<-which(names(rstack)==paste0("burnable",Years[yr]))
    fllow<-which(names(rstack)==paste0("fallow",Years[yr]))
    unbrnable<-which(names(rstack)==paste0("Unburnable",Years[yr]))
    
    rstack[[Lyrs+yr]][]<-ifelse(!is.na(rstack[[ag]][]),1,
                                ifelse(!is.na(rstack[[brnable]][]),2,
                                       ifelse(!is.na(rstack[[fllow]][]),3,
                                              ifelse(!is.na(rstack[[unbrnable]][]),4,NA))))
    
  }
  nnb<-which(names(rstack)=="NEWBurn.1")
  names(rstack[[nnb]])<-"NEWBurn"
  
  #add a total burn layer
  rstack$TotalBurn<-0
  rstack$TotalBurn[which(rstack[["burnable2018"]][]==1 &is.na(rstack[[which(names(rstack)==paste0("burnable",YearsPast2018+2018))]][])==TRUE)]<-1L
  
  
  rstack<<-rstack
} 


###
#plotfun<-function(x){
#r <- x
#r[]<-as.factor(r[])
#rat <- levels(r)[[1]]
#rat[["landcover"]] <- c("Productive","Fallow","Convertable", "Non-convertable")
#levels(r) <- rat

## Plot
#cols<-c("#ffd966","#b6d7a8ff","#7f6000","#969696")
#cols<-c("#7f6000","#b6d7a8ff","#7f6000","#969696") #keep if all rotational ag should be the same color
#rasterVis::levelplot(r, col.regions=cols, xlab="", ylab="")}



#x<-stack(rstack$LndCvr2018,rstack$LndCvr2019,rstack$LndCvr2020,rstack$LndCvr2021)
#animate(x,col.regions=cols, xlab="", ylab="")
###


#LU_AMB(YearsPast2018 = 2, #years (timesteps) to run model
#      Wards = c("Makangale","Msuka Magharibi","Msuka Mashariki"),  #character vector or wards to model. Default is full model
#     FallowTime = 3, #time (in years) it takes for fallow land to recharge
#    AgLimit = 2

#)
#names(rstack)
#raster::plot(rstack$burnable2019)
#raster::plot(rstack$burnable2020)
#raster::plot(rstack$burnable2021)


