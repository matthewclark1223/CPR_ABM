x<-read.csv("~/Pemba_Project/WDPA_Jul2020-csv.csv")
#let's cut down the data so that it's faster to work with
dat<-x[,c(12,13,15,17,18,20)]
dat$MARINE<-as.character(dat$MARINE)
dat$MARINE<-dplyr::recode(dat$MARINE, "0" = "Terrestrial", 
                          "1" = "Coastal","2"="Marine", .default = NA_character_)



library(tidyverse)
library(scales)
library(extrafont)

p2<-dat%>%filter(NO_TK_AREA>0)%>%filter(STATUS_YR>1970)%>%group_by(STATUS_YR)%>%
  mutate(avgSize=mean(NO_TK_AREA))%>%
  ggplot(.,aes(x=STATUS_YR,y=avgSize))+
  geom_jitter(shape=21,size=1.5,stroke=1.5,fill="#a6bddb",
              color="#0570b0",alpha=0.75,width=1.5,height=3000)+
  geom_smooth(se=F,color="black",size=2)+
  theme_classic()+
  scale_y_continuous(label=comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text( size=14, color="black",face="bold"),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12),
        axis.text=(element_text(color="black", size=12)),text=element_text(family="Calibri"),
        legend.title = element_text(colour="black", size=12),
        legend.text = element_text( size = 12),
        legend.position = "none")+
  xlab("Year Established")+ylab("No-Take Conservation Area (Km2)")+ggtitle("New No-Take Area Size")

p1<-dat%>%filter(NO_TAKE =="All")%>%filter(STATUS_YR>1970)%>%group_by(STATUS_YR)%>%
  count()%>%
  ggplot(.,aes(x=STATUS_YR,y=n))+
  geom_point(shape=21,size=1.5,stroke=1.5,fill="#a6bddb",
             color="#0570b0",alpha=0.75)+
  geom_smooth(se=F,color="black",size=2,method="lm")+
  theme_classic()+
  scale_y_continuous(label=comma)+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text( size=14, color="black",face="bold"),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12),
        axis.text=(element_text(color="black", size=12)),
        legend.title = element_text(colour="black", size=12),
        legend.text = element_text( size = 12),text=element_text(family="Calibri"),
        legend.position = "none")+
  xlab("Year")+ylab("Number of Newly Established \n No-Take Conservation Areas")+ggtitle("Newly Established No-Take Conservation Areas")



png(filename = "DissertationProposal/Figures/Ch1/Number.png", pointsize=10, width=900, height=480, res=200)
p1
dev.off()

png(filename = "DissertationProposal/Figures/Ch1/Size.png", pointsize=10, width=900, height=480, res=200)
p2
dev.off()


PaSize = seq(0,1,0.01)
ResourceMobility =seq(0,1,0.01)
fakeData<-expand.grid( Y=ResourceMobility,X=PaSize)
names(fakeData)<-c("PaSize","ResourceMobility")
fakeData$Adoption<-rep(NA,nrow(fakeData))

for (i in 1:nrow(fakeData)){
  
  fakeData[i,]$Adoption<-ifelse(  fakeData[i,]$PaSize>0.2&fakeData[i,]$PaSize<0.9&fakeData[i,]$ResourceMobility>0.35&fakeData[i,]$ResourceMobility<0.85,
                                  rbeta(1,2,2),rbeta(1,2,5))}

for (i in 1:nrow(fakeData)){
  fakeData[i,]$Adoption<-ifelse(fakeData[i,]$PaSize>0.4&fakeData[i,]$PaSize<0.7&fakeData[i,]$ResourceMobility>0.55&fakeData[i,]$ResourceMobility<0.7,
                                rbeta(1,5,1),fakeData[i,]$Adoption)}


names(fakeData)<-c("PaSize","ResourceMobility","Adoption")

p2<-ggplot(data=fakeData, mapping=aes(x=PaSize, y=ResourceMobility, fill= Adoption)) + 
  geom_tile()+scale_fill_viridis()+theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text( size=14, color="black",face="bold"),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12),
        axis.text=(element_text(color="black", size=12)),text=element_text(family="Calibri"),
        legend.title = element_text(colour="black", size=12),
        legend.text = element_text( size = 12))+
  xlab("Percent of Resource Protected")+ylab("Probability of Resource Mobility")+ggtitle("Multiple Groups")

png(filename = "DissertationProposal/Figures/Ch1/abmSim2.png", pointsize=10, width=900, height=680, res=200)
p2
dev.off()


for (i in 1:nrow(fakeData)){
  
  fakeData[i,]$Adoption<-ifelse(  fakeData[i,]$PaSize>0.35&fakeData[i,]$PaSize<0.75&fakeData[i,]$ResourceMobility>0.6&fakeData[i,]$ResourceMobility<0.85,
                                  rbeta(1,2,2),rbeta(1,2,5))}

for (i in 1:nrow(fakeData)){
  fakeData[i,]$Adoption<-ifelse(fakeData[i,]$PaSize>0.45&fakeData[i,]$PaSize<0.6&fakeData[i,]$ResourceMobility>0.65&fakeData[i,]$ResourceMobility<0.75,
                                rbeta(1,5,1),fakeData[i,]$Adoption)}


names(fakeData)<-c("PaSize","ResourceMobility","Adoption")

p1<-ggplot(data=fakeData, mapping=aes(x=PaSize, y=ResourceMobility, fill= Adoption)) + 
  geom_tile()+scale_fill_viridis()+theme_classic()+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        plot.title = element_text( size=14, color="black",face="bold"),
        axis.title.x = element_text( size=12),
        axis.title.y = element_text( size=12),
        axis.text=(element_text(color="black", size=12)),text=element_text(family="Calibri"),
        legend.title = element_text(colour="black", size=12),
        legend.text = element_text( size = 12))+
  xlab("Percent of Resource Protected")+ylab("Probability of Resource Mobility")+ggtitle("Single Group")

png(filename = "DissertationProposal/Figures/Ch1/abmSim1.png", pointsize=10, width=900, height=680, res=200)
p1
dev.off()
