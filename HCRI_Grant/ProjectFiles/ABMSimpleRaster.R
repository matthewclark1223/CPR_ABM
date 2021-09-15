
mBurn2019<-matrix(sample(size=100,0:1,replace=T,prob=c(0.9,0.1)),nrow=10)
rBurn2019 <- raster::raster(mBurn2019)
#plot(rBurn2019)

mProbBurn<-matrix(rbeta(100,1,3),nrow=10)
rProbBurn<-raster(mProbBurn)
#plot(rProbBurn)

rstack<-stack(rBurn2019,rProbBurn )
names(rstack)<-c("Burn2019","ProbBurn") #set the names
rstack

burned2019<-which(rstack$Burn2019[]==1) #get cell numbers that burned in 2019

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
