library(tidyverse)
S<-readRDS("~/Pemba_Project/HCRI_Grant/ProjectFiles/S.RDS")
data<-as.data.frame(S)
names(data)[1:2]<-c("Shehia","prior")

mytheme<-theme(axis.title = element_text(color="black",size=20),
                axis.text = element_text(color="black",size=16),
                axis.ticks  = element_line(color="black"))

data%>%filter(Shehia=="Fundo")%>%
ggplot(.)+
  geom_histogram(aes(x=prior),fill="#525252",color="#d9d9d9")+theme_classic()+mytheme+
  scale_x_continuous(breaks=seq(0,30,5),labels = paste0(seq(0,30,5),"%") )+
  xlab("Yearly forest cover loss caused by external drivers")+ylab("Runs")

