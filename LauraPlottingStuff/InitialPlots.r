x<-read.csv("~/Pemba Project/WDPA_Jul2020-csv.csv")
#let's cut down the data so that it's faster to work with
dat<-x[,c(12,13,15,17,18,20)]
names(dat)
dat$MARINE<-as.character(dat$MARINE)
dat$MARINE<-dplyr::recode(dat$MARINE, "0" = "Terrestrial", 
                            "1" = "Coastal","2"="Marine", .default = NA_character_)



library(tidyverse)

#all
dat%>%filter(STATUS_YR>1970)%>%
ggplot(.,aes(x=STATUS_YR,y=REP_AREA))+
  geom_point(aes(color=MARINE))+
  geom_smooth(color="black",se=F)+theme_classic()+
  facet_wrap(~MARINE,scales="free")+
  ggtitle("Trends in Protected Area Size Over Time")

#yearly median
dat%>%filter(STATUS_YR>1970)%>%group_by(MARINE,STATUS_YR)%>%
  summarise(mean_size = mean(REP_AREA),med_size=median(REP_AREA))%>%
ggplot(.,aes(x=STATUS_YR,y=med_size))+
  geom_point(aes(color=MARINE))+
  geom_smooth(color="black",se=F)+theme_classic()+
  facet_wrap(~MARINE,scales="free")+
  ggtitle("Trends in Protected Area Size Over Time")


## no take

#all
dat%>%filter(STATUS_YR>1970)%>%
  ggplot(.,aes(x=STATUS_YR,y=NO_TK_AREA))+
  geom_point(aes(color=MARINE))+
  geom_smooth(color="black",se=F)+theme_classic()+
  facet_wrap(~MARINE,scales="free")+
  ggtitle("Trends in NO TAKE Protected Area Size Over Time")

#yearly median
dat%>%filter(STATUS_YR>1970)%>%
  group_by(MARINE,STATUS_YR)%>%
  summarise(mean_size = mean(REP_AREA),med_size=median(REP_AREA))%>%
  ggplot(.,aes(x=STATUS_YR,y=med_size))+
  geom_point(aes(color=MARINE))+
  geom_smooth(aes(color=MARINE),se=F)+theme_classic()+
  ggtitle("Trends in NO TAKE Protected Area (Park) Size Over Time")

#working landscape

#all'
dat%>%filter(STATUS_YR>1970)%>%filter(NO_TAKE =="None")%>%
  ggplot(.,aes(x=STATUS_YR,y=REP_AREA))+
  geom_point(aes(color=MARINE))+
  geom_smooth(aes(color=MARINE),se=F)+theme_classic()+
  ggtitle("Trends in WORKING LS Protected Area (Park) Size Over Time")

#yearly median
dat%>%filter(STATUS_YR>1970)%>%filter(NO_TAKE =="None")%>%
  group_by(MARINE,STATUS_YR)%>%
  summarise(mean_size = mean(REP_AREA),med_size=median(REP_AREA))%>%
  ggplot(.,aes(x=STATUS_YR,y=med_size))+
  geom_point(aes(color=MARINE))+
  geom_smooth(aes(color=MARINE),se=F)+theme_classic()+
  ggtitle("Trends in WORKING LS Protected Area Size Over Time")



old<-dat%>%filter(STATUS_YR %in% c(1970:1979))%>%
  group_by(MARINE)%>%
  summarise(Working=mean(REP_AREA),No_Take=mean(NO_TK_AREA))
old$Decade<-rep("1970s",3)

Now<-dat%>%filter(STATUS_YR %in% c(2010:2019))%>%
  group_by(MARINE)%>%
  summarise(Working=mean(REP_AREA),No_Take=mean(NO_TK_AREA))
Now$Decade<-rep("2010s",3)
sizedat<-rbind(old,Now)
sizedat<-gather(sizedat,key="Type",value="Size",Working:No_Take)



ggplot(sizedat,aes(x=Decade,y=Size))+
  geom_point(aes(shape=Type,color=MARINE),size=4)+
  facet_wrap(~MARINE)+ylab("Average Area (square km)")+
  theme_classic()



  sizedat%>%filter(Type =="No_Take")%>%
  ggplot(.,aes(x=Decade,y=Size))+
    geom_point(aes(color=MARINE),size=4)+
    geom_line()+
    facet_wrap(~MARINE)+ylab("Average Area (square km)")+
  theme_bw()

  
  cur_dat<-dat%>%filter(STATUS_YR>1950)
  cur_dat$Decade<-ifelse(cur_dat$STATUS_YR %in% c(1950:1959),"1950s",
                  ifelse(cur_dat$STATUS_YR %in% c(1960:1969),"1960s",
                  ifelse(cur_dat$STATUS_YR %in% c(1970:1979),"1970s",
                  ifelse(cur_dat$STATUS_YR %in% c(1980:1989),"1980s",
                  ifelse(cur_dat$STATUS_YR %in% c(1990:1999),"1990s",
                  ifelse(cur_dat$STATUS_YR %in% c(2000:2010),"2000s",
                  ifelse(cur_dat$STATUS_YR %in% c(2010:2019),"2010s",NA
                                )))))))
  
  
ggplot(cur_dat,aes(x=Decade,y=REP_AREA))+geom_boxplot(aes(fill=MARINE))+
  facet_wrap(~MARINE,scales="free")+scale_y_log10()


ggplot(cur_dat,aes(x=Decade,y=NO_TK_AREA))+geom_boxplot(aes(fill=MARINE))+
  facet_wrap(~MARINE,scales="free")+scale_y_log10()



















