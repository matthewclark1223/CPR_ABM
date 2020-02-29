#stoveID
#copiedFrom
#location
#VisSimModel
#FuelEfficiency
#MonthBuilt
#YearBuilt
#UsePerDay
#Use

namevec<-vector()
for (i in 1:100){
  namevec[i]<-paste0("ID",i)}


#edit this code to add in geographic,genetic, and social relatedness to 
#the stove/stove builder the stove was copied from
#any other variables that might be good to collect?

#could we make these variables in any better way?
dat<-data.frame(stoveID=namevec,
  copiedFrom=c(rep("Original",10),sample(namevec[1:10],90,replace = T)),
  location = rep(NA,100),
  VisSimModel = rbeta(100,35,75),
  BoilTime = sort(rnorm(100,10,2)),
  WaterVol = runif(100,15,20),
  WoodStart = runif(100,300,600),
  WoodEnd = runif(100,50,250),
  MonthBuilt= sample(1:12,100,replace=T),
  YearBuilt= c(rep(2015,5),rep(2017,5),sample(2010:2020,90,replace=T),
  UsePerDay = sort(rpois(100,2)),
  Use = sample(c("Commercial","Family","Both"),100,replace=T)))

head(dat)
