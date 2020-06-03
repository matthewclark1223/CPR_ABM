PaSize = seq(0,1,0.01)
ResourceMobility =seq(0,1,0.01)
fakeData<-expand.grid(X=PaSize, Y=ResourceMobility)
fakeData$DegredationWorking<-sort(rbeta(nrow(fakeData), 2,2))
names(fakeData)<-c("PaSize","ResourceMobility","DegredationWorking")
library(ggplot2)
library(viridis)
ggplot(data=fakeData, mapping=aes(x=PaSize, y=ResourceMobility, fill= DegredationWorking)) + 
            geom_tile()+scale_fill_viridis()



initOutputSuccess<-read.csv("~/Pemba Project/ModelResultsSuccessBias.csv")
initOutputConform<-read.csv("~/Pemba Project/ModelResultsConformityBias.csv")


ggplot(data=initOutputSuccess, mapping=aes(x=PercProtected, y=ResourceMobility, fill= ProtectedCC)) + 
  geom_tile()+scale_fill_viridis()+ggtitle("Success Bias -- Protected CC")

ggplot(data=initOutputConform, mapping=aes(x=PercProtected, y=ResourceMobility, fill= ProtectedCC)) + 
  geom_tile()+scale_fill_viridis()+ggtitle("Conform Bias -- Protected CC")


ggplot(data=initOutputSuccess, mapping=aes(x=PercProtected, y=ResourceMobility, fill= WorkingCC)) + 
  geom_tile()+scale_fill_viridis()+ggtitle("Success Bias -- Working CC")

ggplot(data=initOutputConform, mapping=aes(x=PercProtected, y=ResourceMobility, fill= WorkingCC)) + 
  geom_tile()+scale_fill_viridis()+ggtitle("Conform Bias -- Working CC")

ggplot(data=initOutputSuccess, mapping=aes(x=PercProtected, y=ResourceMobility, fill= MeanPayoff)) + 
  geom_tile()+scale_fill_viridis()+ggtitle("Success Bias -- Mean Payoff")

ggplot(data=initOutputConform, mapping=aes(x=PercProtected, y=ResourceMobility, fill= MeanPayoff)) + 
  geom_tile()+scale_fill_viridis()+ggtitle("Conform Bias -- Mean Payoff")

ggplot(data=initOutputSuccess, mapping=aes(x=PercProtected, y=ResourceMobility, fill= meanTimeWorking)) + 
  geom_tile()+scale_fill_viridis()+ggtitle("Success Bias -- Cooperation")

ggplot(data=initOutputConform, mapping=aes(x=PercProtected, y=ResourceMobility, fill= meanTimeWorking)) + 
  geom_tile()+scale_fill_viridis()+ggtitle("Conform Bias -- Cooperation")





