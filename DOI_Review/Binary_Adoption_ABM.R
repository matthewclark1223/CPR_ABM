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
    
    
    
    
    #give payments to enrolled individuals
    LastTimeAgents$PayoffTotalLastTime<-LastTimeAgents$PayoffTotalLastTime+(PaymentAmount*LastTimeAgents$Enrolled)
    
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
    recProb<-0.01 #0.1
    
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
    if(NewWorkingResourcesTotal==0){NewWorkingResourcesTotal<-as.integer(TotalCCResourceWorking*0.01)}
    if(NewProtectedResourcesTotal==0){NewProtectedResourcesTotal<-as.integer(TotalCCResourceProtected*0.01)}
    
    
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
    
    output$Enrolled[t] <- sum(agents$Enrolled)  #fill it with the outouts from time 1
    output$NumResourcesProtect[t] <- NewProtectedResourcesTotal-sum(agents$PayoffProtectedLastTime) 
    output$NumResourcesWorking[t]  <- NewWorkingResourcesTotal-sum(agents$PayoffWorkingLastTime)
    output$percCCProtect[t]  <- output$NumResourcesProtect[t]/TotalCCResourceProtected
    output$percCCWorking[t]  <- output$NumResourcesWorking[t]/TotalCCResourceWorking
    output$meanPayoff[t]  <- mean(agents$PayoffTotalLastTime)
    
  }
  
  
  p1<-ggplot(data=output,aes(x=timeStep))+
    geom_line(aes(y=percCCProtect),size=3,color="#b2df8a")+
    geom_line(aes(y=percCCWorking),size=3,color="#1f78b4")+
    ylim(0, 1.0)+theme_classic()+ylab("Percent Carrying Capacity")+
    scale_colour_manual(name = 'Area', 
                        values =c('#b2df8a'='#b2df8a','#1f78b4'='#1f78b4'), labels = c('Protected Area','Working Landscape'))+
    ggtitle("Resource Degredation")
  
  
  p2<-ggplot(data=output,aes(x=timeStep))+
    geom_line(aes(y=meanPayoff),size=3,color="#993404")+
    theme_classic()+ylab("Resource Units")+ylim(0,harvestMax+1)+
    ggtitle("Mean Individual Payoff")
  
  p3<-ggplot(data=output,aes(x=timeStep))+
    geom_line(aes(y=Enrolled),size=3,color="#756bb1")+
    theme_classic()+ylab("Mean Percent Time in Working landscape")+
    ggtitle("Number Enrolled")
  
  x<-gridExtra::grid.arrange(p1,p2,p3,ncol=1)
  
  
  return(output) 
}


set.seed(2) #whole group
#set.seed(6) #small groups

deqdatPOS_Conform<-abm(#Specified parameters
  Individuals=100, #number of total resource users in a population
  TotalCarryingCapacity=100000, #total available resource units
  StartPercCarryingCapacity = 0.25, #amount of resources available in the landscape at the start in proportion to CC
  PercProtected=0.2, #percent of the total resource that's in a protected area
  EnrollPercStart=0.01, #percent of individuals who start by following the rules at t0
  LearningStrategy = "Conformist Bias", #options are Success Bias & Conformist...not implementing this...for now
  BiasStrength = 1.05,
  TimeSteps=100,
  ResourceRegenerationPerTimeStep=1.15,
  harvestMax=35,
  ProbOfMobility=0.9)

set.seed(1) #group level
set.seed(1) #small groups
deqdatNEG_Conform<-abm(#Specified parameters
  Individuals=100, #number of total resource users in a population
  TotalCarryingCapacity=100000, #total available resource units
  StartPercCarryingCapacity = 0.25, #amount of resources available in the landscape at the start in proportion to CC
  PercProtected=0.2, #percent of the total resource that's in a protected area
  EnrollPercStart=0.5, #percent of individuals who start by following the rules at t0
  LearningStrategy = "Conformist Bias", #options are Success Bias & Conformist...not implementing this...for now
  BiasStrength = 1.05,
  TimeSteps=100,
  ResourceRegenerationPerTimeStep=1.15,
  harvestMax=35,
  ProbOfMobility=0.9)

set.seed(1)
abm(#Specified parameters
  Individuals=100, #number of total resource users in a population
  
  TotalCarryingCapacity=10000, #total available resource units
  
  StartPercCarryingCapacity = 0.20, #amount of resources available in the landscape at the start in proportion to CC
  
  PaymentAmount = 1, 
  
  
  PercProtected=0.20, #percent of the total resource that's in a protected area
  
  EnrollPercStart=0.02, #percent of individuals who start by following the rules at t0
  
  LearningStrategy = "Success Bias", #options are Success Bias & Conformist...not implementing this...for now
  
  BiasStrength = 1.1,
  
  TimeSteps=100,
  ResourceRegenerationPerTimeStep=1.10, #.10
  harvestMax=2, #20
  ProbOfMobility=0.3)



set.seed(2)
#stable dynamics
abm(#Specified parameters
  Individuals=100, #number of total resource users in a population
  
  TotalCarryingCapacity=10000, #total available resource units
  
  StartPercCarryingCapacity = 0.20, #amount of resources available in the landscape at the start in proportion to CC
  
  PaymentAmount = 9, 
  
  
  PercProtected=0.20, #percent of the total resource that's in a protected area
  
  EnrollPercStart=0.02, #percent of individuals who start by following the rules at t0
  
  LearningStrategy = "Success Bias", #options are Success Bias & Conformist...not implementing this...for now
  
  BiasStrength = 1.1,
  
  TimeSteps=100,
  ResourceRegenerationPerTimeStep=1.5, #.10
  harvestMax=20, #20
  ProbOfMobility=0.3)
