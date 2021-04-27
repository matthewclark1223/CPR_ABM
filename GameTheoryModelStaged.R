library(tidyverse)
library(viridis)
GT_ABM<-function(CarryingCapacityLandscape=10000,
                 Users=100,
                 StartingIntegrity=0.75,
                 PercProtec=0.2,
                 Regrowth=1.1,
                 HarvestLim=8,
                 Timestps=25,
                 HarvestSuccessProb=0.75){
 
  ##start
  CCProt<-CarryingCapacityLandscape*PercProtec
  CCWrk<-CarryingCapacityLandscape*(1-PercProtec)        
  
  RsrcProt<-rep(NA,Timestps)
  RsrcWrk<-rep(NA,Timestps)
  RsrcProt[1]<-CCProt*StartingIntegrity
  RsrcWrk[1]<-CCWrk*StartingIntegrity
  
  
  
  ProtUtil<-rep(NA,Timestps)
  WorkUtil<-rep(NA,Timestps)
  
  
  
  ProtUtil[1]<-(RsrcProt[1]/Users)*HarvestSuccessProb
  WorkUtil[1]<-(RsrcWrk[1]/Users)*HarvestSuccessProb
  
  dfr<-data.frame(RsrcProt,RsrcWrk,ProtUtil,WorkUtil)
  
  
  for (t in 2:Timestps){
    
    if (t %in% 10:14){
    PercProtec = 0.3
    CCWrk = CarryingCapacityLandscape*(1-PercProtec) 
    CCProt = CarryingCapacityLandscape*PercProtec
    }
   
    if (t %in% 15:29){
      PercProtec = 0.4
      CCWrk = CarryingCapacityLandscape*(1-PercProtec) 
      CCProt = CarryingCapacityLandscape*PercProtec
    }
 
    if (t %in% 20:Timestps){
      PercProtec = 0.5
      CCWrk = CarryingCapacityLandscape*(1-PercProtec) 
      CCProt = CarryingCapacityLandscape*PercProtec
    }
  
    wrkRsrcInt<-dfr[t-1,]$RsrcWrk - HarvestLim*Users #subtract the extracted resources from the working landscape
    
    wrkRsrcInt<-ifelse(wrkRsrcInt<0,0,wrkRsrcInt)  #make sure it doesn't go below 0      
    
    wrkRsrcInt<-wrkRsrcInt*Regrowth #regrow resources, we'll limit to the cc later
    
    protRsrcInt<-dfr[t-1,]$RsrcProt * Regrowth #regrow resources, we'll limit to the cc later
    
    protOver<-protRsrcInt-CCProt #the over produced resources in the protected area
    
    protOver<-ifelse(protOver>0,protOver,0) #make sure it's not negative
    
    wrkRsrcInt<-wrkRsrcInt+protOver #add 100% of the resources produced in excess in the protected area.
    
    wrkRsrcInt<-as.integer(ifelse(wrkRsrcInt>CCWrk,CCWrk,wrkRsrcInt)) #limit to the CC
    
    protRsrcInt<-as.integer(ifelse(protRsrcInt>CCProt,CCProt,protRsrcInt))#limit to the cc
    
    dfr[t,]$RsrcProt<-protRsrcInt
    dfr[t,]$RsrcWrk<-wrkRsrcInt
    
    dfr[t,]$ProtUtil<-(protRsrcInt/Users)*HarvestSuccessProb
    dfr[t,]$WorkUtil<-(wrkRsrcInt/Users)*HarvestSuccessProb
    
  }
  dfr$Timesteps<-1:Timestps
  dfr<<-dfr
  return(dfr)}


GT_ABM(CarryingCapacityLandscape=10000,
       Users=100,
       StartingIntegrity=0.2,
       Regrowth=1.4,
       PercProtec=0.2,
       HarvestLim=29,
       Timestps=25,
       HarvestSuccessProb=0.75)


p1<-ggplot(dfr,aes(x=Timesteps))+geom_line(aes(y=ProtUtil,color="Protected"),size=2)+geom_line(aes(y=WorkUtil,color="Working"),size=2)+theme_classic()+ylim(0,60)+
  geom_vline(xintercept = 9,linetype="dashed",color="darkgrey")+geom_vline(xintercept = 14,linetype="dashed",color="darkgrey")+
  geom_vline(xintercept = 19,linetype="dashed",color="darkgrey")+ylab("Expected Utility per User")+xlab("Timestep")+
  scale_colour_manual(name="Area",breaks=c("Protected","Working"),values=c("#33a02c","#1f78b4"))+
theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      plot.title = element_text( size=18, color="black",face="bold"),
      axis.title.x = element_text( size=18),
      axis.title.y = element_text( size=18),
      axis.text=(element_text(color="black", size=14)),
      legend.title = element_text(colour="black", size=18),
      legend.text = element_text( size = 14))


