#sensitivity analyses for harvest max
fillData<-expand.grid(ResourceMobility=seq(0.1,0.9,0.1), harvmax=seq(10,90,10))
fillData$ProtectedCC<-rep(NA,nrow(fillData))
fillData$WorkingCC<-rep(NA,nrow(fillData))
fillData$MeanPayoff<-rep(NA,nrow(fillData))
fillData$meanTimeWorking<-rep(NA,nrow(fillData))


for (i in 1:nrow(fillData)){
  abmnp(Runs=2,Individuals = 100,TimeSteps = 50,harvestMax = fillData[i,]$harvmax,
      PercProtected = 0.35,
      ProbOfMobility = fillData[i,]$ResourceMobility)
  fillData[i,3]<-apply(FullOutput$percCCProtect[50,1:2],1,mean) 
  fillData[i,4]<-apply(FullOutput$percCCWorking[50,1:2],1,mean)  
  fillData[i,5]<-apply(FullOutput$meanPayoff[50,1:2],1,mean)  
  fillData[i,6]<-apply(FullOutput$meanTimeWorking[50,1:2],1,mean)   #[Timesteps,1:Runs]
  print(nrow(fillData))
  print(i)
}


ggplot(data=fillData, mapping=aes(x=ResourceMobility, y=harvmax, fill= ProtectedCC)) + 
  geom_tile()+scale_fill_viridis()
ggplot(data=fillData, mapping=aes(x=ResourceMobility, y=harvmax, fill= WorkingCC)) + 
  geom_tile()+scale_fill_viridis()
ggplot(data=fillData, mapping=aes(x=ResourceMobility, y=harvmax, fill= MeanPayoff)) + 
  geom_tile()+scale_fill_viridis()
ggplot(data=fillData, mapping=aes(x=ResourceMobility, y=harvmax, fill= meanTimeWorking)) + 
  geom_tile()+scale_fill_viridis()



#narrow in



fillData<-expand.grid(ResourceMobility=seq(0.1,0.9,0.2), harvmax=seq(10,18,2))
fillData$ProtectedCC<-rep(NA,nrow(fillData))
fillData$WorkingCC<-rep(NA,nrow(fillData))
fillData$MeanPayoff<-rep(NA,nrow(fillData))
fillData$meanTimeWorking<-rep(NA,nrow(fillData))


for (i in 1:nrow(fillData)){
  abmnp(Runs=2,Individuals = 100,TimeSteps = 50,harvestMax = fillData[i,]$harvmax,
      PercProtected = 0.35,
      ProbOfMobility = fillData[i,]$ResourceMobility)
  fillData[i,3]<-apply(FullOutput$percCCProtect[50,1:2],1,mean) 
  fillData[i,4]<-apply(FullOutput$percCCWorking[50,1:2],1,mean)  
  fillData[i,5]<-apply(FullOutput$meanPayoff[50,1:2],1,mean)  
  fillData[i,6]<-apply(FullOutput$meanTimeWorking[50,1:2],1,mean)   #[Timesteps,1:Runs]
  print(nrow(fillData))
  print(i)
}


ggplot(data=fillData, mapping=aes(x=ResourceMobility, y=harvmax, fill= ProtectedCC)) + 
  geom_tile()+scale_fill_viridis()
ggplot(data=fillData, mapping=aes(x=ResourceMobility, y=harvmax, fill= WorkingCC)) + 
  geom_tile()+scale_fill_viridis()
ggplot(data=fillData, mapping=aes(x=ResourceMobility, y=harvmax, fill= MeanPayoff)) + 
  geom_tile()+scale_fill_viridis()
ggplot(data=fillData, mapping=aes(x=ResourceMobility, y=harvmax, fill= meanTimeWorking)) + 
  geom_tile()+scale_fill_viridis()