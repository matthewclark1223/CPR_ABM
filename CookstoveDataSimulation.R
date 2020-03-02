#variables we want to include listed below
#stoveID
#copiedFrom
#location
#VisSimModel
#FuelEfficiency
#MonthBuilt
#YearBuilt
#UsePerDay
#UseTotalEstimate
#UseType
#GeogDistModel
#SocialDistModel
#GeneticDistModel
#GenderBuilder
#GenderModel


#Make a vector of stove Id's
namevec<-vector()
for (i in 1:100){
  namevec[i]<-paste0("ID",i)}



#any other variables that might be good to collect?

#could we make these variables in any better way?
dat<-data.frame(stoveID=namevec, 
  copiedFrom=c(rep("Original",10),sample(namevec[1:10],90,replace = T)),
  location = rep(NA,100),
  VisSimModel = rbeta(100,35,75),
  BoilTime = sort(rnorm(100,10,2)),
  WaterVol = runif(100,15,20),
  WoodStart = sort(runif(100,300,600)),
  WoodEnd = sort(runif(100,50,250)),
  MonthBuilt= sample(1:12,100,replace=T),
  YearBuilt= c(rep(2015,5),rep(2017,5),sample(2016:2020,90,replace=T)),
  UsePerDay = sort(rpois(100,2)),
  UseTotalEstimate = rep(NA,100),
  stdPmEsmssions = rnorm(100,0,1),
  GeogDistModel = sort(rgamma(100,75,6)),
  SocialDistModel = c(rep(1,55),rep(2,30),rep(3,15)),
  GeneticDistModel = rep(NA,100),
  GenderBuilder = sample(c(rep("Female",8),rep("Male",2)),100,replace=T),
  GenderModel =sample(c(rep("Female",8),rep("Male",2)),100,replace=T),
  Use = sample(c("Commercial","Family","Both"),100,replace=T))

head(dat)

write.csv(dat,file="SimulatedStoveData.csv")
