data<-data.frame(CF=sort(rbeta(100,1,2)),WC=sort(rpois(100,7)),GD=rpois(100,5))

CSP1<-ggplot(data,aes(x=WC,y=CF))+
  geom_point(shape=21,size=3,stroke=3,fill="#a6bddb",
                                            color="#0570b0",alpha=0.75)+
  geom_smooth(se=F,color="black",size=2,method="lm")+
  theme_classic()+
  scale_y_continuous(label=comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text( size=18, color="black",face="bold"),
        axis.title.x = element_text( size=18),
        axis.title.y = element_text( size=18),
        axis.text=(element_text(color="black", size=14)),
        legend.title = element_text(colour="black", size=18),
        legend.text = element_text( size = 14),
        legend.position = "none")+
  xlab("Weekly Communication Between Original Stove Builder and Copier")+
  ylab("Copying Fidelity")+ggtitle("Simulated Copying Fidelity vs. Weekly Communication")

CSP2<-ggplot(data,aes(x=WC,y=GD))+
  geom_jitter(shape=21,size=3,stroke=3,fill="#a6bddb",
             color="#0570b0",alpha=0.75)+
  geom_smooth(se=F,color="black",size=2,method="lm")+
  theme_classic()+
  scale_y_continuous(label=comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text( size=18, color="black",face="bold"),
        axis.title.x = element_text( size=18),
        axis.title.y = element_text( size=18),
        axis.text=(element_text(color="black", size=14)),
        legend.title = element_text(colour="black", size=18),
        legend.text = element_text( size = 14),
        legend.position = "none")+
  xlab("Geographic Distance Between Original Stove Builder and Copier")+
  ylab("Copying Fidelity")+ggtitle("Simulated Copying Fidelity vs. Geographic Distance")


gridExtra::grid.arrange(CSP1,CSP2,nrow=1)



SP<-rnorm(100,0,3)
xb<-0+1.5*SP
p<-1/(1+exp(-xb))
y <- rbinom(n = 100, size = 1, prob = p)

plot(y~SP)
data<-data.frame(COF=y,SelPres=SP)

ggplot(data,aes(x=SelPres,y=COF))+
  geom_point(shape=21,size=3,stroke=3,fill="#a6bddb",
              color="#0570b0",alpha=0.75)+
  geom_smooth(se=F,color="black",size=2)+
  theme_classic()+
  scale_y_continuous(breaks=c(0,1))+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text( size=18, color="black",face="bold"),
        axis.title.x = element_text( size=18),
        axis.title.y = element_text( size=18),
        axis.text=(element_text(color="black", size=14)),
        legend.title = element_text(colour="black", size=18),
        legend.text = element_text( size = 14),
        legend.position = "none")+
  xlab("Calculated Selection Pressure for Costly Conservation Adoption")+
  ylab("Protected Area Observed (Y/N)")+ggtitle("Simulated Theoretical Selection Pressure vs. Actual Protected Area Adoption")



PaSize = seq(0,1,0.01)
ResourceMobility =seq(0,1,0.01)
fakeData<-expand.grid(X=PaSize, Y=ResourceMobility)
fakeData$DegredationWorking<-rbeta(nrow(fakeData), 2,2)
names(fakeData)<-c("PaSize","ResourceMobility","DegredationWorking")

ggplot(data=fakeData, mapping=aes(x=PaSize, y=ResourceMobility, fill= DegredationWorking)) + 
  geom_tile()+scale_fill_viridis(direction=-1)


PaSize = seq(0,1,0.01)
ResourceMobility =seq(0,1,0.01)
fakeData<-expand.grid( Y=ResourceMobility,X=PaSize)

eta <- c(0.01, 0.2, 0.6)
N <- nrow(fakeData)
x <- PaSize
l<-ResourceMobility
z <- rnorm(N, 0, 2)
mu <- binomial(link = logit)$linkinv(eta[1]  + eta[2]*x+ eta[3]*l)
phi <- 500
y <- rbeta(N, mu * phi, (1 - mu) * phi)

for (i in 1:nrow(fakeData)){

fakeData[i,]$Adoption<-ifelse(  fakeData[i,]$PaSize>0.2&fakeData[i,]$PaSize<0.9&fakeData[i,]$ResourceMobility>0.45&fakeData[i,]$ResourceMobility<1,
                          rbeta(1,2,2),rbeta(1,2,5))}

for (i in 1:nrow(fakeData)){
  fakeData[i,]$Adoption<-ifelse(fakeData[i,]$PaSize>0.4&fakeData[i,]$PaSize<0.7&fakeData[i,]$ResourceMobility>0.65&fakeData[i,]$ResourceMobility<0.9,
    rbeta(1,5,1),fakeData[i,]$Adoption)}


names(fakeData)<-c("PaSize","ResourceMobility","Adoption")

ggplot(data=fakeData, mapping=aes(x=PaSize, y=ResourceMobility, fill= Adoption)) + 
  geom_tile()+scale_fill_viridis()+theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                                        plot.title = element_text( size=18, color="black",face="bold"),
                                                        axis.title.x = element_text( size=18),
                                                        axis.title.y = element_text( size=18),
                                                        axis.text=(element_text(color="black", size=14)),
                                                        legend.title = element_text(colour="black", size=18),
                                                        legend.text = element_text( size = 14))+
  xlab("Percent of Resource Protected")+ylab("Probability of Resource Mobility")+ggtitle("Simulated Protected Area Adoption Rate - Multiple Groups")



for (i in 1:nrow(fakeData)){
  
  fakeData[i,]$Adoption<-ifelse(  fakeData[i,]$PaSize>0.2&fakeData[i,]$PaSize<0.9&fakeData[i,]$ResourceMobility>0.45&fakeData[i,]$ResourceMobility<1,
                                  rbeta(1,2,2),rbeta(1,2,2))}

for (i in 1:nrow(fakeData)){
  fakeData[i,]$Adoption<-ifelse(fakeData[i,]$PaSize>0.4&fakeData[i,]$PaSize<0.7&fakeData[i,]$ResourceMobility>0.65&fakeData[i,]$ResourceMobility<0.9,
                                rbeta(1,2,2),fakeData[i,]$Adoption)}


names(fakeData)<-c("PaSize","ResourceMobility","Adoption")

ggplot(data=fakeData, mapping=aes(x=PaSize, y=ResourceMobility, fill= Adoption)) + 
  geom_tile()+scale_fill_viridis()+theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text( size=18, color="black",face="bold"),
        axis.title.x = element_text( size=18),
        axis.title.y = element_text( size=18),
        axis.text=(element_text(color="black", size=14)),
        legend.title = element_text(colour="black", size=18),
        legend.text = element_text( size = 14))+
  xlab("Percent of Resource Protected")+ylab("Probability of Resource Mobility")+ggtitle("Simulated Protected Area Adoption Rate - Single Group")



