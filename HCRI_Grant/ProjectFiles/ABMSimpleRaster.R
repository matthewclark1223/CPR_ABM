#There are three raster layers going into this model:
#-2019 burn data
#-Probability of burn (based off of regression on soil type, slope, & distance to road)
#-2018 Land cover data (aggregated classes are Burnable, Agriculture, Bare land/fallow, Unburnable)

#

#




mBurn2019<-matrix(sample(size=1000,0:1,replace=T,prob=c(0.9,0.1)),nrow=10)
rBurn2019 <- raster::raster(mBurn2019)
#plot(rBurn2019)

mProbBurn<-matrix(rbeta(1000,1,3),nrow=10)
rProbBurn<-raster(mProbBurn)
#plot(rProbBurn)

classes<-factor(c("Agriculture","Bare","Burnable","Unburnable"))
levels(classes)
              
mLndCvr<-matrix(sample(size=1000,as.integer(classes),replace=T,prob=c(0.1,0.1,0.7,0.1)),nrow=10)
rLndCvr<-raster::raster(mLndCvr)

#stack all starting layers together
rstack<-stack(rBurn2019,rProbBurn,rLndCvr )
names(rstack)<-c("Burn2019","ProbBurn","LndCvr2018") #set the names
rstack$LndCvr2018<- as.integer(rstack$LndCvr2018) #this should be an integer

ag2018<-which(rstack$LndCvr2018[]==1) #ag from 2018 land cover layer
burned2019<-which(rstack$Burn2019[]==1) #get cell numbers that burned in 2019
bare2018<-which(rstack$LndCvr2018[]==2) #fallow/bare from 2018 land cover layer
burnable2018<-which(rstack$LndCvr2018[]==3) #burnable from 2018 landcover layer
unburnable2018<-which(rstack$LndCvr2018[]==4) #unburnable from 2018 land cover layer

#Make the starting ag layer
rstack$ag2019<-NA #Make all NA to start
rstack$ag2019[ag2018]<-2 #plots that were already ag in 2018 have been at for 2 years
rstack$ag2019[burned2019]<-1

#Make starting fallow layer
rstack$fallow2019<-NA #Make all NA to start
rstack$fallow2019[bare2018]<-sample(2:3,size=length(bare2018),replace=T)

#Make starting burnable layer
 burnable2019<-burnable2018[!burnable2018 %in% burned2019] #remove the ones that already burned
 rstack$burnable2019<-NA #make allNA to start
 rstack$burnable2019[burnable2019]<-1 #make 1 if burnable
 
 #Make starting UNburnable layer
 rstack$Unburnable2019<-NA
 rstack$Unburnable2019[unburnable2018]<-1

 
 #make a container vector for the 2020 burn predictions
 rstack$predBurn2020<-0 #create a 0's raster with same extent as existing
 
 
 


for(i in burned2019){

neighborCells<-adjacent(rstack$Burn2019, cells=i, directions=8, pairs=TRUE)[,2] 
mostlikelyBurn<-which(rstack$ProbBurn[neighborCells] == max(rstack$ProbBurn[neighborCells]))
spr<-neighborCells[mostlikelyBurn]
if(rstack$Burn2019[spr]==0){rstack$predBurn2020[spr]<-1}
}
plot(rstack)

rstackNAs<-rstack
rstackNAs[rstackNAs<=0]<-NA
r_df<-raster::as.data.frame(rstackNAs, xy = TRUE) 

ggplot()+
  geom_tile(data = na.omit(r_df%>%dplyr::select(Burn2019,x,y)) , 
            aes(x = x, y = y),fill="red") +
  geom_tile(data = na.omit(r_df%>%dplyr::select(predBurn2020,x,y)) , 
            aes(x = x, y = y),fill="darkred") +
  
  theme_bw()
