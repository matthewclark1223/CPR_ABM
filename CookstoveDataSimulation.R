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
for (i in 1:40){
  namevec[i]<-paste0("ID",i)}



#Make survey data frame
Sdat<-data.frame(#initial survey data
                S1.1.0 = rep("MattC",40),
                S1.1.1 = sample( seq(as.Date("2020-10-30"), as.Date("2021-01-10"), by="days"),40,replace = T),
                S1.1.2 =  sample(c(chron::times("09:00:00"),chron::times("12:00:00"),chron::times("15:00:00")),40,replace=T),
                S1.1.3 =  sample(c(chron::times("10:00:00"),chron::times("15:00:00"),chron::times("17:00:00")),40,replace=T),
                S1.1.4 =  sample(c("Kifundi","Makangale","Mjini","Wingwi"),40,replace=T),
                S1.1.5 =  rep("XXX",40),
                S1.1.6 = namevec ,
                #equity tool
                S1.2.0 = as.logical(sample(0:1,40,replace=T)) ,
                S1.2.1 = as.logical(sample(0:1,40,replace=T)) ,
                S1.2.2 = as.logical(sample(0:1,40,replace=T)) ,
                S1.2.3 = as.logical(sample(0:1,40,replace=T)) ,
                S1.2.4 = as.logical(sample(0:1,40,replace=T)) ,
                S1.2.5 = sample(c("Earth_Sand","Cement_Concrete","other"),40,replace=T,prob = c(.5,.5,.1)) ,
                S1.2.6 = sample(c("Cement","other"),40,replace=T,prob = c(.75,.1)) ,
                S1.2.7 = sample(c("Iron","Grass_thatch","other"),40,replace=T,prob = c(.5,.5,.1)) ,
                S1.2.8 = sample(c("Firewood","Charcoal","other"),40,replace=T,prob = c(.5,.5,.1)) ,
                S1.2.9 = sample(c("Electricity","Battery_solar","other"),40,replace=T,prob = c(.5,.5,.1)) ,
                #cookstove info
                S1.3.0 =  as.logical(sample(0:1,40,replace=T,prob = c(.1,.9))),
                S1.3.1 =  rep(NA,40), #come back make match with above (i.e. cant have 1.3.0 == YES and 1.3.1 == NGO)
                S1.3.2 =  rep(NA,40), #come back make NGO built stoves earlier year than others
                S1.3.3 =  sample(seq(1:12),40,replace=T), 
                S1.3.4 =  sample(seq(0:12),40,replace=T),
                S1.3.5 =  sample(c("Family","Commercial","Both"),40,replace=T,prob = c(.8,.05,.2)),
                S1.3.6 =  sample(seq(0:5),40,replace=T),
                S1.3.7 =  sample(seq(0:50),40,replace=T),
                #copying information
                S1.4.0 =  rep(NA,40),  #come back make match with NGO built vs not (i.e. NGO built stoves should not be copied)
                S1.4.1 =  rep("XXX",40), 
                S1.4.2 =  rep(NA,40), #come back make match with NGO built vs not
                S1.4.3 =  rep(NA,40), #come back make match with NGO built vs not 
                S1.4.4 =  rep(NA,40), #come back make match with NGO built vs not
                #secondary copying information
                S1.5.0 =  as.logical(sample(0:1,40,replace=T,prob = c(.7,.2))),
                S1.5.1 =  rep("XXX",40), 
                S1.5.2 =  rep(NA,40), #come back make match with copied or not (1.5.0)
                S1.5.3 =  rep(NA,40), #come back make match with copied or not (1.5.0)
                S1.5.4 =  rep(NA,40)) #come back make match with copied or not (1.5.0)
                
                
                
            
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
  stdPmEmissions = rnorm(100,0,1),
  GeogDistModel = sort(rgamma(100,75,6)),
  SocialDistModel = c(rep(1,55),rep(2,30),rep(3,15)),
  GeneticDistModel = rep(NA,100),
  GenderBuilder = sample(c(rep("Female",8),rep("Male",2)),100,replace=T),
  GenderModel =sample(c(rep("Female",8),rep("Male",2)),100,replace=T),
  Use = sample(c("Commercial","Family","Both"),100,replace=T))

head(dat)
write.csv(dat,file="SimulatedStoveData.csv")


dat$totwoodburned<-dat$WoodStart-dat$WoodEnd

library(ggplot2)

ggplot(data=dat,aes(x=as.character(YearBuilt), y=stdPmEmissions))+
  geom_boxplot()+
  geom_jitter(color="purple")

ggplot(data=dat,aes(x=VisSimModel, y=stdPmEmissions))+
  geom_line()


ggplot(data=dat,aes(x=YearBuilt, y=stdPmEmissions))+
  geom_smooth(method="lm",se=F)
#PM emissions by the year built. 














