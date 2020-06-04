

#using 0.6 as the starting % cc. REsults show that other parameters have greatest effect from 0.5 to 0.7


FillData<-expand.grid(PercProtected=seq(0.05,0.95,0.05), ResourceMobility=c(0.01,0.5,0.99))
FillData$ProtectedCC<-rep(NA,nrow(FillData))
FillData$WorkingCC<-rep(NA,nrow(FillData))
FillData$MeanPayoff<-rep(NA,nrow(FillData))
FillData$meanTimeWorking<-rep(NA,nrow(FillData))


for (i in 1:nrow(FillData)){
  abmnp(Runs=5,Individuals = 100,TimeSteps = 50,harvestMax = 15,StartPercCarryingCapacity = 0.6,
      PercProtected = FillData[i,]$PercProtected, 
      ProbOfMobility = FillData[i,]$ResourceMobility)
  FillData[i,3]<-apply(FullOutput$percCCProtect[50,1:5],1,mean) 
  FillData[i,4]<-apply(FullOutput$percCCWorking[50,1:5],1,mean)  
  FillData[i,5]<-apply(FullOutput$meanPayoff[50,1:5],1,mean)  
  FillData[i,6]<-apply(FullOutput$meanTimeWorking[50,1:5],1,mean)   #[Timesteps,1:Runs]
  print(nrow(FillData))
  print(i)
}

Res1_succ<-FillData
write.csv(Res1_succ,file="rename1.csv")
Res1_succ<-read.csv("~/Pemba Project/rename1.csv")
Res1_succ$ResourceMobility<-ifelse(Res1_succ$ResourceMobility ==0.01, "NonMobile",
                           ifelse(Res1_succ$ResourceMobility==0.50,"ModerateMobility","HighlyMobile"))

p1<-ggplot(Res1_succ,aes(x=PercProtected,y=ProtectedCC,color=ResourceMobility))+
  geom_point()+geom_line()+theme_classic()+mytheme
p2<-ggplot(Res1_succ,aes(x=PercProtected,y=WorkingCC,color=ResourceMobility))+
  geom_point()+geom_line()+theme_classic()+mytheme
p3<-ggplot(Res1_succ,aes(x=PercProtected,y=MeanPayoff,color=ResourceMobility))+
  geom_point()+geom_line()+theme_classic()+mytheme
p4<-ggplot(Res1_succ,aes(x=PercProtected,y=meanTimeWorking,color=ResourceMobility))+
  geom_point()+geom_line()+theme_classic()+mytheme
gridExtra::grid.arrange(p1,p2,p3,p4, nrow=2)

#conformity bias
FillData<-expand.grid(PercProtected=seq(0.05,0.95,0.05), ResourceMobility=c(0.01,0.5,0.99))
FillData$ProtectedCC<-rep(NA,nrow(FillData))
FillData$WorkingCC<-rep(NA,nrow(FillData))
FillData$MeanPayoff<-rep(NA,nrow(FillData))
FillData$meanTimeWorking<-rep(NA,nrow(FillData))


for (i in 1:nrow(FillData)){
  abmnp(Runs=5,Individuals = 100,TimeSteps = 50,harvestMax = 15,StartPercCarryingCapacity = 0.6, LearningStrategy = "Conformist",
        PercProtected = FillData[i,]$PercProtected, 
        ProbOfMobility = FillData[i,]$ResourceMobility)
  FillData[i,3]<-apply(FullOutput$percCCProtect[50,1:5],1,mean) 
  FillData[i,4]<-apply(FullOutput$percCCWorking[50,1:5],1,mean)  
  FillData[i,5]<-apply(FullOutput$meanPayoff[50,1:5],1,mean)  
  FillData[i,6]<-apply(FullOutput$meanTimeWorking[50,1:5],1,mean)   #[Timesteps,1:Runs]
  print(nrow(FillData))
  print(i)
}

Res1_conf<-FillData
write.csv(Res1_conf,file="rename2.csv")
Res1_conf<-read.csv("~/Pemba Project/rename2.csv")
Res1_conf$ResourceMobility<-ifelse(Res1_conf$ResourceMobility ==0.01, "NonMobile",
                                   ifelse(Res1_conf$ResourceMobility==0.50,"ModerateMobility","HighlyMobile"))

ggplot(Res1_conf,aes(x=PercProtected,y=ProtectedCC,color=ResourceMobility))+
  geom_point()+geom_line()
ggplot(Res1_conf,aes(x=PercProtected,y=WorkingCC,color=ResourceMobility))+
  geom_point()+geom_line()
ggplot(Res1_conf,aes(x=PercProtected,y=MeanPayoff,color=ResourceMobility))+
  geom_point()+geom_line()
ggplot(Res1_conf,aes(x=PercProtected,y=meanTimeWorking,color=ResourceMobility))+
  geom_point()+geom_line()











#environmental degredation and PA size success
FillData<-expand.grid(PercProtected=seq(0.1,0.9,0.1), StartPercCC=seq(0.1,0.9,0.1))
FillData$ProtectedCC<-rep(NA,nrow(FillData))
FillData$WorkingCC<-rep(NA,nrow(FillData))
FillData$MeanPayoff<-rep(NA,nrow(FillData))
FillData$meanTimeWorking<-rep(NA,nrow(FillData))


for (i in 1:nrow(FillData)){
  abm(Runs=5,Individuals = 100,TimeSteps = 50,harvestMax = 15,
      PercProtected = FillData[i,]$PercProtected, StartPercCarryingCapacity =  FillData[i,]$StartPercCC,
      ProbOfMobility = 0.5)
  FillData[i,3]<-apply(FullOutput$percCCProtect[50,1:5],1,mean) 
  FillData[i,4]<-apply(FullOutput$percCCWorking[50,1:5],1,mean)  
  FillData[i,5]<-apply(FullOutput$meanPayoff[50,1:5],1,mean)  
  FillData[i,6]<-apply(FullOutput$meanTimeWorking[50,1:5],1,mean)   #[Timesteps,1:Runs]
  print(nrow(FillData))
  print(i)
}

SD_Success<-FillData
write.csv(SD_Success,file="rename3.csv")
SD_Success<-read.csv("~/Pemba Project/rename3.csv")

ggplot(data=SD_Success, mapping=aes(x=PercProtected, y=StartPercCC, fill= ProtectedCC)) + 
  geom_tile()+scale_fill_viridis()
ggplot(data=SD_Success, mapping=aes(x=PercProtected, y=StartPercCC, fill= WorkingCC)) + 
  geom_tile()+scale_fill_viridis()
ggplot(data=SD_Success, mapping=aes(x=PercProtected, y=StartPercCC, fill= MeanPayoff)) + 
  geom_tile()+scale_fill_viridis()
ggplot(data=SD_Success, mapping=aes(x=PercProtected, y=StartPercCC, fill= meanTimeWorking)) + 
  geom_tile()+scale_fill_viridis()


#environmental degredation and PA size conform

FillData<-expand.grid(PercProtected=seq(0.1,0.9,0.1), StartPercCC=seq(0.1,0.9,0.1))
FillData$ProtectedCC<-rep(NA,nrow(FillData))
FillData$WorkingCC<-rep(NA,nrow(FillData))
FillData$MeanPayoff<-rep(NA,nrow(FillData))
FillData$meanTimeWorking<-rep(NA,nrow(FillData))


for (i in 1:nrow(FillData)){
  abm(Runs=5,Individuals = 100,TimeSteps = 50,harvestMax = 15,LearningStrategy = "Conformist",
      PercProtected = FillData[i,]$PercProtected, StartPercCarryingCapacity =  FillData[i,]$StartPercCC,
      ProbOfMobility = 0.5)
  FillData[i,3]<-apply(FullOutput$percCCProtect[50,1:5],1,mean) 
  FillData[i,4]<-apply(FullOutput$percCCWorking[50,1:5],1,mean)  
  FillData[i,5]<-apply(FullOutput$meanPayoff[50,1:5],1,mean)  
  FillData[i,6]<-apply(FullOutput$meanTimeWorking[50,1:5],1,mean)   #[Timesteps,1:Runs]
  print(nrow(FillData))
  print(i)
}

SD_Conform<-FillData
write.csv(SD_Conform,file="rename4.csv")
SD_Conform<-read.csv("~/Pemba Project/rename4.csv")
ggplot(data=SD_Conform, mapping=aes(x=PercProtected, y=StartPercCC, fill= ProtectedCC)) + 
  geom_tile()+scale_fill_viridis()
ggplot(data=SD_Conform, mapping=aes(x=PercProtected, y=StartPercCC, fill= WorkingCC)) + 
  geom_tile()+scale_fill_viridis()
ggplot(data=SD_Conform, mapping=aes(x=PercProtected, y=StartPercCC, fill= MeanPayoff)) + 
  geom_tile()+scale_fill_viridis()
ggplot(data=SD_Conform, mapping=aes(x=PercProtected, y=StartPercCC, fill= meanTimeWorking)) + 
  geom_tile()+scale_fill_viridis()

















