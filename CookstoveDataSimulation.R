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
                S1.4.4a =  rep(NA,40),#come back
                S1.4.4b =  rep(NA,40),#come back 
                #secondary copying information
                S1.5.0 =  as.logical(sample(0:1,40,replace=T,prob = c(.7,.2))),
                S1.5.1 =  rep("XXX",40), 
                S1.5.2 =  rep(NA,40), #come back make match with copied or not (1.5.0)
                S1.5.3 =  rep(NA,40), #come back make match with copied or not (1.5.0)
                S1.5.4a =  rep(NA,40),#come back 
                S1.5.4b =  rep(NA,40)) #come back 
                
#1.3.1
for (i in 1:length(Sdat$S1.3.1)){
  Sdat[i,]$S1.3.1<- ifelse(Sdat[i,]$S1.3.0 == FALSE,sample(c("NGO","Friend","Family"),1,prob =c(0.8,0.1,0.1) ),NA)  
}
#1.3.2            
for (i in 1:length(Sdat$S1.3.2)){
    Sdat[i,]$S1.3.2<-ifelse(is.na(Sdat[i,]$S1.3.1) ,sample(2016:2020,1),2015)   
  } 
#1.4.0
Sdat$S1.4.0<-ifelse(is.na(Sdat$S1.3.1),TRUE,           
                    ifelse(Sdat$S1.3.1 == "NGO",FALSE,TRUE)) 
#1.4.2
for (i in 1:length(Sdat$S1.4.2)){
  Sdat[i,]$S1.4.2<-ifelse(Sdat[i,]$S1.4.0 == TRUE ,sample(c("Friend","Family"),1),NA)   
} 
#1.4.3
for (i in 1:length(Sdat$S1.4.3)){
  Sdat[i,]$S1.4.3<-ifelse(Sdat[i,]$S1.4.0 == TRUE ,rpois(1,3),NA)   
}

#1.4.4a
for (i in 1:length(Sdat$S1.4.4a)){
  Sdat[i,]$S1.4.4a<-ifelse(Sdat[i,]$S1.1.4 == "Kifundi",sample(seq(from =-4.985137,to=-4.954661,by=0.000002),1),
                           ifelse(Sdat[i,]$S1.1.4 == "Makangale",sample(seq(from =-4.907181,to=-4.9038961,by=0.000002),1),
                                 sample(seq(from =-5.000792,to=-4.978561,by=0.000002),1)))
                                         
}
#1.4.4b
for (i in 1:length(Sdat$S1.4.4b)){
  Sdat[i,]$S1.4.4b<-ifelse(Sdat[i,]$S1.1.4 == "Kifundi",sample(seq(from =39.716989,to=39.751064,by=0.000002),1),
                           ifelse(Sdat[i,]$S1.1.4 == "Makangale",sample(seq(from =39.687118,to=39.691136,by=0.000002),1),
                                  sample(seq(from =39.826496,to=39.853104,by=0.000002),1)))
  
}



#1.5.2
for (i in 1:length(Sdat$S1.5.2)){
  Sdat[i,]$S1.5.2<-ifelse(Sdat[i,]$S1.5.0 == TRUE ,sample(c("Friend","Family"),1),NA)   
} 

#1.5.3
for (i in 1:length(Sdat$S1.5.3)){
  Sdat[i,]$S1.5.3<-ifelse(Sdat[i,]$S1.5.0 == TRUE ,rpois(1,3),NA)   
}

#1.5.4a
for (i in 1:length(Sdat$S1.5.4a)){
  Sdat[i,]$S1.5.4a<-ifelse(Sdat[i,]$S1.1.4 == "Kifundi",sample(seq(from =-4.985137,to=-4.954661,by=0.000002),1),
                           ifelse(Sdat[i,]$S1.1.4 == "Makangale",sample(seq(from =-4.907181,to=-4.9038961,by=0.000002),1),
                                  sample(seq(from =-5.000792,to=-4.978561,by=0.000002),1)))
  
}
#1.5.4b
for (i in 1:length(Sdat$S1.5.4b)){
  Sdat[i,]$S1.5.4b<-ifelse(Sdat[i,]$S1.1.4 == "Kifundi",sample(seq(from =39.716989,to=39.751064,by=0.000002),1),
                           ifelse(Sdat[i,]$S1.1.4 == "Makangale",sample(seq(from =39.687118,to=39.691136,by=0.000002),1),
                                  sample(seq(from =39.826496,to=39.853104,by=0.000002),1)))
  
}



#Make efficiency test data frame
Tdat<-data.frame(#initial stove data
T.S.1 = rep("MattC",40),
T.S.2 = Sdat[,2],
T.S.3 = Sdat[,7],
T.S.4 = sample(4:10,40,replace=T),
T.S.5 = rpois(40,30),
T.S.6 = rpois(40,5),
T.S.7 = rpois(40,5),
T.S.8 = rpois(40,5),
T.S.9 = rpois(40,17),
T.S.10 = rep("XXX",40), #calc
T.S.11 = rpois(40,22),
T.S.12 = rep(0.5,40),
T.S.13 = rep(1.5,40),

#test outputs
T.1.1 =  rep(chron::times("11:00:00"),40),
T.1.2 =  rep(chron::times("11:00:00")+chron::times("00:30:00"),40),
T.1.3 =  rpois(10,5), 
T.1.4 =  rpois(40,100),
T.1.5 =  rep("XXX",40), #make a result of starting pot + water

#Stove dimesnions
T.2.1 =  sample(45:60,40,replace=T),
T.2.2 =  sample(100:150,40,replace=T),
T.2.3 =  sample(45:60,40,replace=T),
T.2.4 =  as.logical(sample(0:1,40,replace=T)),
T.2.5 =  sample(10:20,40,replace=T),
T.2.6 =  sample(10:20,40,replace=T),
T.2.7 =  sample(c("Indoor","Outdoor"),40,replace=T),
T.2.8 =sample(c("Sepia", "Penny", "Russet","Tawny", "Saddle" ),40,replace=T)  
)
Tdat$T.S.10<-WaterStart(Tdat$T.S.9)
Tdat$T.1.5<-(Tdat$T.S.13+Tdat$T.S.10)-sample(rgamma(100,0.8,1),1)

dat<-merge.data.frame(Sdat, Tdat, by.x = "S1.1.6", by.y = "T.S.3")




  















