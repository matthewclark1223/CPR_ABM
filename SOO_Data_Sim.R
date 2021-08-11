#simulate data for the SOO analyses. 

#In each of the 24 communities, want to look at the effects of:
  # T1 - Environmental perceptions
    # T1a - Change in scrub forest
    # T1b - Years to regrow scrub forest (before able to cut)
  # T2 - Social interaction
    # T2a - Perceived stealing from outsiders
    # T2b - Perceived overharvest from within-group
    # T2c - Perceived conservation success of outside groups
  # T3 - Group history with conservation
    # T3a - Past REDD+ "treatment"
    # T3b - CFP outreach

#We want to understand the effects of all of these things on:
  # O1 - Group goals/rules and...
    # O1a - Preferred harvest limit of scrub forest
    # O1b - Consequences for outside stealers
    # 01c - Consequences for within-group overharvesters
  # O2 -...ability to achieve/enforce those goals/rules
    # 02a - Individuals actually coming to a tree planting event 
    # 02b - Have they endogenously started conservation practices

# This script experiments with the sample size needed to recover the parameter values of interest
# Sample size from each community
set.seed(3)
Ssize<-40
StudyWards<-c("Tondooni","Msuka Magharibi","Tumbe Magharibi","Shumba Mjini","Kifundi","Mgogoni",
              "Gando","Mjini Wingwi","Mtambwe Kaskazini","Mtambwe Kusini","Fundo","Mgelema","Kisiwa Panza",
              "Kangani","Changaweni","Kambini","Chumbageni","Shungi","Ziwani","Piki",
              "Ukunjwi","Mjimbini","Michenzani","Kilindi")

Orgdat<-data.frame(Indv=1:(length(StudyWards)*Ssize),
                  Ward=rep(StudyWards,Ssize))


REDDWards<-c("Changaweni", "Fundo", "Gando", "Kambini","Kangani","Kifundi","Kisiwa Panza",
              "Mgelema","Mgogoni","Michenzani","Mjimbini","Mjini Wingwi","Msuka Magharibi","Mtambwe Kaskazini",
              "Mtambwe Kusini","Shumba Mjini","Tondooni","Tumbe Magharibi")


Orgdat$REDD<-Orgdat$Ward%in%REDDWards

set.seed(3)
Orgdat$ScrubChng<-rnorm(Ssize,-0.2,0.1)
Orgdat$ScrubRegrw<-rpois(Ssize,2)
Orgdat$OutSteal<-rpois(Ssize,20)
Orgdat$InOverHarv<-runif(Ssize,0,50)
Orgdat$OutCons<-rpois(Ssize,7)

#Predictions
  # P1 - Perceived degradation and in-group over harvest will cause individuals to push for stricter harvest limits moreso than regrowth rates
      # Preferred harvest limit of scrub forest ~ Change in scrub forest (b1a) + Years to regrow scrub forest (b1b) + Perceived overharvest from within-group (b1c)
  # P2 - Perceived leakage will drive individuals to participate in conservation moreso than observing successful groups
      # Individuals actually coming to a tree planting event  ~ Perceived stealing from outsiders (b2a) + Perceived conservation success of outside groups (b2b) +Past REDD+ "treatment" (b2c)
  


#P1
b1a<-0.5
b1b<--0.7
b1c<--1.0
Intercept1<-3.4  #log(30)

stdize<-function(x){
  (x-mean(x))/(2*sd(x))}

mu1 = Intercept1 + stdize(Orgdat$ScrubChng)*b1a + stdize(Orgdat$REDD)*b1b + stdize(Orgdat$OutSteal)*b1c
Orgdat$PrefHarvLim<-rpois(n=length(mu1),lambda=exp(mu1))
hist(Orgdat$PrefHarvLim)
fit1<-rstanarm::stan_glm(PrefHarvLim~stdize(ScrubChng)+stdize(REDD)+stdize(OutSteal),family="poisson",data=Orgdat,chains=1)
coef(fit1)
summary(fit1)

z1<-as.data.frame(fit1)
colnames(z1) <- c("Intercept", "Outgroup Theft", "REDD+","Forest Cover Change")

mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                plot.title = element_text( size=18, color="black",face="bold"),
                axis.title.x = element_text( size=18),
                axis.title.y = element_text( size=18),
                axis.text=(element_text(color="black", size=14)),
                legend.title = element_text(colour="black", size=18),
                legend.text = element_text( size = 14))



parzSet1<-data.frame(par=c("Intercept", #get all the parameter values on the std scale of the predictor
                          "Outgroup Theft",
                          "REDD+",
                          "Forest Cover Change"),val=c(3.4,
                                             0.5,
                                             -0.7,
                                             -1.0))

z1<-stack(z1)%>%group_by(ind)%>%
  summarise(lower=quantile(values,.25),
            upper=quantile(values,.75),
            top = quantile(values,.95),
            bottom = quantile(values,.05),
            mid=quantile(values,.5))%>%filter(ind%in%parzSet1$par)
ggplot(z1)+ggtitle("")+
  geom_pointrange(mapping=aes(y = mid ,x=as.character(ind),ymin=bottom,ymax=top),position = position_dodge(width = 0.5),size=1,alpha=0.8)+xlab("Predictor")+
  geom_point(data=parzSet1,aes(x=as.character(par),y=val),shape=24,alpha=0.5,size=9,color="blue",fill="blue")+
  ylab("Parameter Value")+
  coord_flip()+
  theme_classic()+mytheme+theme(strip.background = element_blank(),
                                strip.text.x = element_blank())+
  theme(legend.position = "bottom")




#P2
b2a<--1
b2b<--0.15
b2c<--2.5
Intercept2<-0.15 

stdize<-function(x){
  (x-mean(x))/(2*sd(x))}

mu2<-Intercept2 + stdize(Orgdat$OutSteal)*b2a + stdize(Orgdat$REDD)*b2b + stdize(Orgdat$ScrubChng)*b2c
p <- 1/(1 + exp(-mu2))

Orgdat$Plant<- rbinom(n = length(mu2), size = 1, prob = p)
hist(Orgdat$Plant)

fit2<-rstanarm::stan_glm(Plant~stdize(OutSteal)+stdize(REDD)+stdize(ScrubChng),family="binomial",data=Orgdat,chains=1 )
coef(fit2)

z2<-as.data.frame(fit2)
colnames(z2) <- c("Intercept", "Outgroup Theft", "REDD+","Forest Cover Change")

parzSet2<-data.frame(par=c("Outgroup Theft", #get all the parameter values on the std scale of the predictor
                    "REDD+",
                    "Forest Cover Change",
                    "Intercept"),val=c(-1,
                                         -0.15,
                                         -2.5,
                                         0.15))

z2<-stack(z2)%>%group_by(ind)%>%
  summarise(lower=quantile(values,.25),
            upper=quantile(values,.75),
            top = quantile(values,.95),
            bottom = quantile(values,.05),
            mid=quantile(values,.5))
  ggplot(z2)+ggtitle("")+
  geom_pointrange(mapping=aes(y = mid ,x=as.character(ind),ymin=bottom,ymax=top),position = position_dodge(width = 0.5),size=1,alpha=0.8)+xlab("Predictor")+
   geom_point(data=parzSet2,aes(x=as.character(par),y=val),shape=24,alpha=0.5,size=9,color="blue",fill="blue")+
  ylab("Parameter Value")+
  coord_flip()+
  theme_classic()+mytheme+theme(strip.background = element_blank(),
                                strip.text.x = element_blank())+
  theme(legend.position = "bottom")



