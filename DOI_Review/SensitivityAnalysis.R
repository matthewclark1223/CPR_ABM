#####Scheduling
#Establish a degraded resource
#set some of that into a conservation area
#Establish a population
#Some % of the population buys into the conservation project (low %)
#That % gets a payment every time step..maybe once?...maybe every 12 time steps?
#People not enrolled harvest from the entire landscape
#people enrolled harvest from just the un-conserved area. 
#At the end of each time step, everyone learns via success biased imitation (equation from equifinality paper)
#People either enroll of unenroll
#Measure the number of people enrolled/unenrolled
#Measure the % CC of the resource

#repeat for 1:t
library(ggplot2)
#####

abm<-function(#Specified parameters
  Individuals=100, #number of total resource users in a population
  
  TotalCarryingCapacity=10000, #total available resource units
  
  StartPercCarryingCapacity = 0.1, #amount of resources available in the landscape at the start in proportion to CC
  
  PaymentAmount = 5, 
  
  
  PercProtected=0.3, #percent of the total resource that's in a protected area
  
  EnrollPercStart=0.05, #percent of individuals who start by following the rules at t0
  
  LearningStrategy = "Success Bias", #options are Success Bias & Conformist...not implementing this...for now
  
  BiasStrength = 1.5,
  
  TimeSteps=20,
  ResourceRegenerationPerTimeStep=1.15,
  harvestMax=25,
  ProbOfMobility=0.5){
  
  ## Set parameters
  PercWorking= 1-PercProtected #percent of resource in a working landscape
  TotalCCResourceProtected=PercProtected*TotalCarryingCapacity #total resource units in carrying capacity of protected area
  TotalCCResourceWorking=PercWorking*TotalCarryingCapacity #total resource units in carrying capacity of working area
  StartResourceWorking = TotalCCResourceWorking*StartPercCarryingCapacity #number of resources in the working landscape starting
  StartResourceProtected = TotalCCResourceProtected*StartPercCarryingCapacity #number of resources in the protected landscape starting
  EnrollNumStart= as.integer(EnrollPercStart*Individuals) #number of individuals cooperating fully at t0
  NotEnrollNumStart= Individuals - EnrollNumStart#number of individuals defecting at t0
  Enrolled = c(rep(1,EnrollNumStart),rep(0,NotEnrollNumStart))#enrolled or not
  
  PercTimeProtected = PercProtected*(1-Enrolled) #percent of their foraging time each indv spends in the PA. For those not enrolled this is = the the % protected..Treat whole landscape equally
  PercTimeWorking = (1-PercTimeProtected)#percent of their foraging time each indv spends in the working landscape
  
  agents<-data.frame(Enrolled,
                     PercTimeProtected,
                     PercTimeWorking,
                     PayoffProtectedLastTime = rep(NA,Individuals),
                     PayoffWorkingLastTime= rep(NA,Individuals))   #dataframe to be filled with initial payoffs
  
  ProtectPerDefect<-ifelse(NotEnrollNumStart ==0,as.integer(StartResourceProtected),as.integer(StartResourceProtected/NotEnrollNumStart)) #protected area resources per each individual harvesting there
  WorkingPerTotal<-as.integer(StartResourceWorking/Individuals) #working landscape resources per individual
  
  
  for ( i in 1:nrow(agents)){  #fill the starting df percent payoff from the protected landscape is a function of the amount 
    agent2<-agents[i,]   #of resources there per individual searching there, and a random draw with a chance of success equal to time spent
    agents[i,5]<-rbinom(1,WorkingPerTotal,agent2$PercTimeWorking)
    agents[i,5]<-ifelse(agents[i,5]>=harvestMax,harvestMax,agents[i,5])
    agents[i,4]<- ifelse(agents[i,5]< harvestMax,rbinom(1,ProtectPerDefect,agent2$PercTimeProtected),0) #if they didnt get the max harvest, then 
    agents[i,4]<-ifelse(agents[i,4]>harvestMax-agents[i,5],harvestMax-agents[i,5],agents[i,4])
  } #see what they get from the working landscape first
  
  
  agents$PayoffTotalLastTime<- agents$PayoffProtectedLastTime + agents$PayoffWorkingLastTime #total payoff is the sum of what they get from both areas
  
  
  output<-data.frame(timeStep = 1:TimeSteps, #create df to fill with the outputs from each time step
                     Enrolled = rep(NA,TimeSteps),
                     NumResourcesProtect = rep(NA,TimeSteps),
                     percCCProtect = rep(NA,TimeSteps),
                     NumResourcesWorking = rep(NA,TimeSteps),
                     percCCWorking = rep(NA,TimeSteps),
                     meanPayoff = rep(NA,TimeSteps))
  
  output$Enrolled[1] <- sum(agents$Enrolled)  #fill it with the outouts from time 1
  output$NumResourcesProtect[1] <- StartResourceProtected-sum(agents$PayoffProtectedLastTime) 
  output$NumResourcesWorking[1] <- StartResourceWorking-sum(agents$PayoffWorkingLastTime) 
  output$percCCProtect[1]  <- output$NumResourcesProtect[1]/TotalCCResourceProtected
  output$percCCWorking[1]  <- output$NumResourcesWorking[1]/TotalCCResourceWorking
  output$meanPayoff[1]  <- mean(agents$PayoffTotalLastTime) 
  
  
  for (t in 2:TimeSteps){  #run the abm for all of the time steps
    
    #choose new strategy  
    
    LastTimeAgents<-agents  #save the agents from the previous time to a new df
    
    if(sum(LastTimeAgents$Enrolled)==Individuals){ #if there's no one NOT enrolled, unenroll someone at random
      LastTimeAgents[sample(1:Individuals,1),"Enrolled"]<-0
    }
    
    if(sum(LastTimeAgents$Enrolled)==0){ #if there's no one  enrolled, enroll someone at random
      LastTimeAgents[sample(1:Individuals,1),"Enrolled"]<-1
      
    }
    
    
    
    
    
    succLearn<-function(agentdf){
      #meanPayoffEnroll
      if(sum(agentdf$Enrolled)==0){
        payMeanEnrl<-0}
      if(sum(agentdf$Enrolled)!=0){
        payMeanEnrl<-mean(agentdf[agentdf$Enrolled==1,]$PayoffTotalLastTime)}
      
      #meanPayoffUnenroll
      if(sum(agentdf$Enrolled)==nrow(agentdf)){
        payMeanNOTEnrl<-0}
      if(sum(agentdf$Enrolled)!=nrow(agentdf)){
        payMeanNOTEnrl<-mean(agentdf[agentdf$Enrolled==0,]$PayoffTotalLastTime)} 
      
      probEnroll<-exp(BiasStrength*payMeanEnrl)/
        (exp(BiasStrength*payMeanEnrl)+exp(BiasStrength*payMeanNOTEnrl))
      
      agentdf$Enrolled<-rbinom(nrow(agentdf),1, probEnroll)
      return(agentdf$Enrolled)
    }
    
    
    
    #need to change this so agents can spend varying portions of time in each zone
    
    if(LearningStrategy == "Success Bias"){
      
      #success biased leaning accross the whole group
      #NewEnroll<-succLearn(LastTimeAgents)
      #success biased learning within small groups
      NewEnroll<-rep(NA,Individuals)
      groups<-split(sample(1:Individuals),f=1:50)
      
      
      for(j in 1:length(groups)){
        group<-LastTimeAgents[groups[[j]],]      
        groupEnroll<-succLearn(group)  
        NewEnroll[groups[[j]]]<-groupEnroll
      }
      
    } #end success biased learning
    
    ###OLD one from Barrett. This does not model disease dynamics
    #ConfLearn<-function(agentdf){
    
    
    #probEnroll<-(length(which(agentdf$Enrolled==1)))^BiasStrength/
    #((length(which(agentdf$Enrolled==1)))^BiasStrength+(length(which(agentdf$Enrolled==0)))^BiasStrength)
    
    #apply to ALL agents
    # agentdf$Enrolled<-rbinom(nrow(agentdf),1, probEnroll)
    
    #apply to only unenrolled
    # agentsdf$Enrolled<-ifelse(agentsdf$Enrolled == 0, rbinom(1,1, probEnroll),1)
    
    #return(agentdf$Enrolled)
    #}
    
    #BETTER implementation. disease spread
    
    
    
    #start conformist learning
    if(LearningStrategy=="Conformist Bias"){
      
      spreadProb<-0.25 #0.25
      recProb<-0.03 #0.01
      
      NewEnroll<-rep(NA,Individuals)
      
      
      for(a in 1:nrow(LastTimeAgents)){
        thisagent<-LastTimeAgents[a,]
        otheragents<-LastTimeAgents[-a,]
        pairagent<-otheragents[sample(1:nrow(otheragents),1),]
        if(thisagent$Enrolled == 1){    NewEnroll[a]<-rbinom(1,1,1-recProb)}
        if(thisagent$Enrolled == 0){    NewEnroll[a]<-ifelse(pairagent$Enrolled ==1,
                                                             rbinom(1,1,spreadProb),0 )}
        
      }
      
      
      
    }
    
    #end conformist learning
    
    PercTimeProtected = PercProtected*(1-NewEnroll) #percent of their foraging time each indv spends in the PA. For those not enrolled this is = the the % protected..Treat whole landscape equally
    #PercTimeProtected = output$NumResourcesProtect[t-1]/(output$NumResourcesProtect[t-1]+output$NumResourcesWorking[t-1])
    
    PercTimeWorking = (1-PercTimeProtected)
    
    
    agents<-data.frame(Enrolled=NewEnroll,
                       PercTimeProtected,   #new agent dataframe to fill
                       PercTimeWorking,
                       PayoffProtectedLastTime = rep(NA,Individuals),
                       PayoffWorkingLastTime= rep(NA,Individuals)) 
    
    
    #New resource pools to pull from
    #Resources over the CC move with probability 'ProbOfMobility'
    
    NewProtectedResourcesTotal<-round(output$NumResourcesProtect[t-1] * ResourceRegenerationPerTimeStep,digits=0)
    ProtectedResourcesOverCC<-round(NewProtectedResourcesTotal-TotalCCResourceProtected,digits=0)
    ProtectedResourcesOverCC<- ifelse(ProtectedResourcesOverCC<=  0,0, ProtectedResourcesOverCC)
    
    NewWorkingResourcesTotal<-round(output$NumResourcesWorking[t-1] * ResourceRegenerationPerTimeStep,digits=0)
    WorkingResourcesOverCC<-round(NewWorkingResourcesTotal-TotalCCResourceWorking,digits=0)
    WorkingResourcesOverCC<- ifelse(WorkingResourcesOverCC<=  0,0, WorkingResourcesOverCC)
    
    
    ##Resource mobility
    
    
    #all resources mobile
    #LeaveWorking<-rbinom(1,NewWorkingResourcesTotal,ProbOfMobility) #number of resources which leave the protected area
    #LeaveProtected<-rbinom(1,NewProtectedResourcesTotal,ProbOfMobility)#number of resources which leave the working area
    
    #make it so only new resources can leave 
    LeaveWorking<-rbinom(1, WorkingResourcesOverCC,ProbOfMobility) #number of resources which leave the protected area
    LeaveProtected<-rbinom(1,ProtectedResourcesOverCC,ProbOfMobility)#number of resources which leave the working area
    
    
    #do the accounting on entering vs leaving individuals
    NewWorkingResourcesTotal<-NewWorkingResourcesTotal+LeaveProtected
    NewProtectedResourcesTotal<-NewProtectedResourcesTotal+LeaveWorking
    
    #Make sure they dont go over the CC again
    NewWorkingResourcesTotal<-ifelse(NewWorkingResourcesTotal<=TotalCCResourceWorking,NewWorkingResourcesTotal,TotalCCResourceWorking)
    NewProtectedResourcesTotal<-ifelse(NewProtectedResourcesTotal<=  TotalCCResourceProtected,NewProtectedResourcesTotal, TotalCCResourceProtected)
    
    #dont let them alll the way to 0. if they do they won't regenerate
    if(NewWorkingResourcesTotal==0){NewWorkingResourcesTotal<-as.integer(TotalCCResourceWorking*0.05)}
    if(NewProtectedResourcesTotal==0){NewProtectedResourcesTotal<-as.integer(TotalCCResourceProtected*0.05)}
    
    
    ##calculate available resources per individual
    if(nrow(agents[agents$PercTimeProtected>0,])==0){ProtectPerDefect<-NewProtectedResourcesTotal}
    if(nrow(agents[agents$PercTimeProtected>0,])!=0){ProtectPerDefect<-as.integer(NewProtectedResourcesTotal/nrow(agents[agents$PercTimeProtected>0,]))}
    WorkingPerTotal<-as.integer(NewWorkingResourcesTotal/Individuals)
    
    
    for ( i in 1:nrow(agents)){  #fill the starting df percent payoff from the protected landscape is a function of the amount 
      agent2<-agents[i,]   #of resources there per individual searching there, and a random draw with a chance of success equal to time spent
      agents[i,5]<-rbinom(1,WorkingPerTotal,agent2$PercTimeWorking)
      agents[i,5]<-ifelse(agents[i,5]>=harvestMax,harvestMax,agents[i,5])
      agents[i,4]<- ifelse(agents[i,5]< harvestMax,rbinom(1,ProtectPerDefect,agent2$PercTimeProtected),0) #if they didnt get the max harvest, then 
      agents[i,4]<-ifelse(agents[i,4]>harvestMax-agents[i,5],harvestMax-agents[i,5],agents[i,4])
    } #see what they get from the working landscape first
    
    
    
    agents$PayoffTotalLastTime<- agents$PayoffProtectedLastTime + agents$PayoffWorkingLastTime #total payoff is the sum of what they get from both areas
    #give payments to enrolled individuals
    agents$PayoffTotalLastTime<-agents$PayoffTotalLastTime+(PaymentAmount*agents$Enrolled)
    
    
    output$Enrolled[t] <- sum(agents$Enrolled)  #fill it with the outouts from time 1
    output$NumResourcesProtect[t] <- NewProtectedResourcesTotal-sum(agents$PayoffProtectedLastTime) 
    output$NumResourcesWorking[t]  <- NewWorkingResourcesTotal-sum(agents$PayoffWorkingLastTime)
    output$percCCProtect[t]  <- output$NumResourcesProtect[t]/TotalCCResourceProtected
    output$percCCWorking[t]  <- output$NumResourcesWorking[t]/TotalCCResourceWorking
    output$meanPayoff[t]  <- mean(agents$PayoffTotalLastTime)
    
  }
  
  
  
  return(output) 
}


#####Multi run to address reviewer comments
# success bias
set.seed(1)


runs<-5

sensData<-expand.grid(PercProtected=seq(0.1,0.9,by=0.2),
                      ResourceRegenerationPerTimeStep=seq(1.1,1.5,by=0.1),
                      MAE=NA)




for(s in 1:nrow(sensData)){

#this gets recreated and filled for each parameter combo
NrunDt<-data.frame()
  
for(r in 1:runs){
  x<-abm(#Specified parameters
    Individuals=100, #number of total resource users in a population
    
    TotalCarryingCapacity=10000, #total available resource units
    
    StartPercCarryingCapacity = 0.20, #amount of resources available in the landscape at the start in proportion to CC
    
    PaymentAmount = 9, 
    
    PercProtected=sensData[s,]$PercProtected, #percent of the total resource that's in a protected area
    
    EnrollPercStart=0.02, #percent of individuals who start by following the rules at t0
    
    LearningStrategy = "Success Bias", #options are Success Bias & Conformist...not implementing this...for now
    
    BiasStrength = 1.1,
    
    TimeSteps=100,
    ResourceRegenerationPerTimeStep=sensData[s,]$ResourceRegenerationPerTimeStep, #.10
    harvestMax=15, #20
    ProbOfMobility=0.3)
  
  x$Run<-rep(r,nrow(x))
  NrunDt<-rbind(NrunDt,x)

}


#Now average across runs

SucBiasdat<-NrunDt%>%group_by(timeStep)%>%summarize(Enrolled=ceiling(median(Enrolled)))

################
###fitting Stan data

###fitting the success bias model with behavior env. feedbacks

sample_days<-30 #30
sample_n<-100
t_max<-100
sample_y<-SucBiasdat[1:sample_days,]$Enrolled
sample_time<-1:sample_days

# Parameters must be stored in a named list.
params <- list(beta = NA, #rate of transmission
               gamma = NA, #rate of dropout
               chi = NA) #time after dropout before willing to try again


stan_d = list(n_obs = sample_days,
              n_params = length(params),
              n_difeq = 3, #number of diff eqs
              n_sample = sample_n,
              n_fake = length(1:t_max),
              y = sample_y,
              t0 = 0,
              ts = sample_time,
              fake_ts = c(1:t_max))

params_monitor = c("y_hat", "y0", "params", "fake_I") #fake_I from generated quantities

# Fit and sample from the posterior
mod = stan("~/Pemba_Project/DOI_Review/SIRS.stan",
           data = stan_d,
           pars = params_monitor,
           chains = 4,
           warmup = 500,
           iter = 1500)



# Extract the posterior samples to a structured list:
posts <- rstan::extract(mod)
#hist(posts$params[,1])
#hist(posts$params[,2])

#R0 number of infected people
#R0<-(posts$params[,1]*0.20)/posts$params[,2]

#length(which(R0<1))/length(R0)

#plot it
# Proportion infected from the synthetic data:
sample_prop = sample_y / sample_n

# Model predictions across the sampling time period.
# These were generated with the "fake" data and time series.
mod_median = apply(posts$fake_I[,,2], 2, median)
mod_low = apply(posts$fake_I[,,2], 2, quantile, probs=c(0.025))
mod_high = apply(posts$fake_I[,,2], 2, quantile, probs=c(0.975))
mod_time = stan_d$fake_ts


# Combine into two data frames for plotting
df_sample = data.frame(sample_prop, sample_time)
df_fit = data.frame(mod_median, mod_low, mod_high, mod_time)

draws<-as_tibble(posts$fake_I[,,2])%>%add_column(draw=1:4000)
names(draws)[1:100]<-1:100

draws <-  pivot_longer(draws, c(1:100) , names_to = "mod_time")

pred<-mod_median[-(1:sample_days)]
obs<-(SucBiasdat[(sample_days+1):sample_n,]$Enrolled)/sample_n

OUTCOME<-Metrics::mae(pred,obs)


sensData[s,]$MAE<-OUTCOME
#sensData[s,]$COR<-cor(pred,obs)
print(s)
}
sensData$MAE<-sensData$MAE*100
sensData$MAEPerc<-paste0((round(sensData$MAE,digits=2)),"%")
ggplot(sensData, aes(x = PercProtected, y = (ResourceRegenerationPerTimeStep-1), fill = MAE/100)) +
  geom_tile(color = "black") +
  theme_classic()+
  theme(panel.background = element_rect(color="white"),
        axis.text=element_text(color="black",size=14),
        axis.title=element_text(color="black",size=18),
        line = element_blank())+
  geom_text(aes(label = MAEPerc), color = "white", size = 4) +
  scale_x_continuous(labels = scales::percent)+
  scale_y_continuous(labels = scales::percent)+
  scale_fill_viridis_c(name="Mean Absolute\nError",
                       direction=-1,option="A" ,begin=0,end=0.75,
                       labels=scales::percent)+
  xlab("Percent of Landscape in Protected Area")+
  ylab("Resoure Regeneration Rate\nper Time Step")




########Now for #2
set.seed(1)


runs<-5

sensData2<-expand.grid(PaymentAmount=seq(4,12,by=2),
                      StartPercCarryingCapacity=seq(0.1,0.9,by=0.2),
                      MAE=NA)




for(s in 1:nrow(sensData2)){
  
  #this gets recreated and filled for each parameter combo
  NrunDt2<-data.frame()
  
  for(r in 1:runs){
    x<-abm(#Specified parameters
      Individuals=100, #number of total resource users in a population
      
      TotalCarryingCapacity=10000, #total available resource units
      
      StartPercCarryingCapacity = sensData2[s,]$StartPercCarryingCapacity, #amount of resources available in the landscape at the start in proportion to CC
      
      PaymentAmount = sensData2[s,]$PaymentAmount, 
      
      PercProtected=0.2, #percent of the total resource that's in a protected area
      
      EnrollPercStart=0.02, #percent of individuals who start by following the rules at t0
      
      LearningStrategy = "Success Bias", #options are Success Bias & Conformist...not implementing this...for now
      
      BiasStrength = 1.1,
      
      TimeSteps=100,
      ResourceRegenerationPerTimeStep=1.3, #.10
      harvestMax=15, #20
      ProbOfMobility=0.3)
    
    x$Run<-rep(r,nrow(x))
    NrunDt2<-rbind(NrunDt2,x)
    
  }
  
  
  #Now average across runs
  
  SucBiasdat2<-NrunDt2%>%group_by(timeStep)%>%summarize(Enrolled=ceiling(median(Enrolled)))
  
  ################
  ###fitting Stan data
  
  ###fitting the success bias model with behavior env. feedbacks
  
  sample_days<-30 #30
  sample_n<-100
  t_max<-100
  sample_y<-SucBiasdat2[1:sample_days,]$Enrolled
  sample_time<-1:sample_days
  
  # Parameters must be stored in a named list.
  params <- list(beta = NA, #rate of transmission
                 gamma = NA, #rate of dropout
                 chi = NA) #time after dropout before willing to try again
  
  
  stan_d = list(n_obs = sample_days,
                n_params = length(params),
                n_difeq = 3, #number of diff eqs
                n_sample = sample_n,
                n_fake = length(1:t_max),
                y = sample_y,
                t0 = 0,
                ts = sample_time,
                fake_ts = c(1:t_max))
  
  params_monitor = c("y_hat", "y0", "params", "fake_I") #fake_I from generated quantities
  
  # Fit and sample from the posterior
  mod = stan("~/Pemba_Project/DOI_Review/SIRS.stan",
             data = stan_d,
             pars = params_monitor,
             chains = 4,
             warmup = 500,
             iter = 1500)
  
  
  
  # Extract the posterior samples to a structured list:
  posts <- rstan::extract(mod)
  #hist(posts$params[,1])
  #hist(posts$params[,2])
  
  #R0 number of infected people
  #R0<-(posts$params[,1]*0.20)/posts$params[,2]
  
  #length(which(R0<1))/length(R0)
  
  #plot it
  # Proportion infected from the synthetic data:
  sample_prop = sample_y / sample_n
  
  # Model predictions across the sampling time period.
  # These were generated with the "fake" data and time series.
  mod_median = apply(posts$fake_I[,,2], 2, median)
  mod_low = apply(posts$fake_I[,,2], 2, quantile, probs=c(0.025))
  mod_high = apply(posts$fake_I[,,2], 2, quantile, probs=c(0.975))
  mod_time = stan_d$fake_ts
  
  
  # Combine into two data frames for plotting
  df_sample = data.frame(sample_prop, sample_time)
  df_fit = data.frame(mod_median, mod_low, mod_high, mod_time)
  
  draws<-as_tibble(posts$fake_I[,,2])%>%add_column(draw=1:4000)
  names(draws)[1:100]<-1:100
  
  draws <-  pivot_longer(draws, c(1:100) , names_to = "mod_time")
  
  pred<-mod_median[-(1:sample_days)]
  obs<-(SucBiasdat2[(sample_days+1):sample_n,]$Enrolled)/sample_n
  
  OUTCOME<-Metrics::mae(pred,obs)
  
  
  sensData2[s,]$MAE<-OUTCOME
  #sensData[s,]$COR<-cor(pred,obs)
  print(s)
}
sensData2$MAE<-sensData2$MAE/10
sensData2$MAEPerc<-paste0((round(sensData2$MAE,digits=2)),"%")
ggplot(sensData2, aes(x = PaymentAmount, y = StartPercCarryingCapacity, fill = MAE/100)) +
  geom_tile(color = "black") +
  theme_classic()+
  theme(panel.background = element_rect(color="white"),
        axis.text=element_text(color="black",size=14),
        axis.title=element_text(color="black",size=18),
        line = element_blank())+
  geom_text(aes(label = MAEPerc), color = "white", size = 4) +
  scale_y_continuous(labels = scales::percent)+
  scale_x_continuous(breaks = c(4,6,8,10,12))+
  scale_fill_viridis_c(name="Mean Absolute\nError",
                       direction=-1,option="A"  ,begin=0,end=0.75,
                       labels=scales::percent)+
  xlab("Payment Ammount for Enrolled Agents")+
  ylab("Resource Starting Percent\nof Carrying Capacity")





