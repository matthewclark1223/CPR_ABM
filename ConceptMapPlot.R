dat<-data.frame(var=c(rep("Protected area resources",20),rep("Working landscape resources",20),rep("Mean harvest",20)),Percentage=sort(rbeta(60,1,2)))


summarySE <- function(data=NULL, measurevar, groupvars=NULL, na.rm=FALSE,
                      conf.interval=.95, .drop=TRUE) {
  library(plyr)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else       length(x)
  }
  
  # This does the summary. For each group's data frame, return a vector with
  # N, mean, and sd
  datac <- ddply(data, groupvars, .drop=.drop,
                 .fun = function(xx, col) {
                   c(N    = length2(xx[[col]], na.rm=na.rm),
                     mean = mean   (xx[[col]], na.rm=na.rm),
                     sd   = sd     (xx[[col]], na.rm=na.rm)
                   )
                 },
                 measurevar
  )
  
  # Rename the "mean" column    
  datac <- rename(datac, c("mean" = measurevar))
  
  datac$se <- datac$sd / sqrt(datac$N)  # Calculate standard error of the mean
  
  # Confidence interval multiplier for standard error
  # Calculate t-statistic for confidence interval: 
  # e.g., if conf.interval is .95, use .975 (above/below), and use df=N-1
  ciMult <- qt(conf.interval/2 + .5, datac$N-1)
  datac$ci <- datac$se * ciMult
  
  return(datac)
}

dat<-summarySE(dat, measurevar="Percentage", groupvars=c("var"))

install.packages("extrafont")
library(extrafont)
library(tidyverse)
font_import()
loadfonts(device="win")       #Register fonts for Windows bitmap output
fonts()  
mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                plot.title = element_text( size=18, color="black",face="bold"),
                axis.title.x = element_text( size=18),
                axis.title.y = element_text( size=18),
                axis.text=(element_text(color="black", size=14)),
                legend.title = element_text(colour="black", size=18),
                legend.text = element_text( size = 14))

windowsFonts(Times=windowsFont("Times New Roman"))
ggplot(dat,aes(x=var,y=Percentage,color=var,group=var)) +
  geom_errorbar(aes(ymin=Percentage-sd, ymax=Percentage+sd,colour=var), width=0) +
  geom_point( size=5, fill="white")+theme_classic()+mytheme+ theme(legend.position = "none")+
  scale_color_viridis_d(option="magma",begin=0,end=0.7)+
  scale_y_continuous(name="Percent", labels = scales::percent)+xlab("Outcome")+theme(text=element_text(family="Times"))

  
  
ggplot(dat,aes(x=var,y=Percentage,color=var,group=var)) +
  facet_grid(~var)+
  geom_errorbar(aes(ymin=Percentage-sd, ymax=Percentage+sd,colour=var), width=0) +
  geom_point( size=5, fill="white")+theme_classic()+mytheme+ theme(legend.position = "none")+
  scale_color_viridis_d(option="magma",begin=0,end=0.7)+
  scale_y_continuous(name="Percent", labels = scales::percent)+xlab("Outcome")+theme(text=element_text(family="Times")) 
  
  
  
  