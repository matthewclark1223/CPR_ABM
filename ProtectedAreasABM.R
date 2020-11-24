library(ggplot2)

abm<-function(#Specified parameters
  Individuals=100, #number of total resource users in a population
  
  TotalCarryingCapacity=10000, #total available resource units
  
  StartPercCarryingCapacity = 0.7, #amount of resources available in the landscape at the start in proportion to CC
  
  PercProtected=0.3, #percent of the total resource that's in a protected area
  
  CoopPercStart=0.9, #percent of individuals who start by following the rules at t0
  
  LearningStrategy = "Success Bias", #options are Success Bias & Conformist
  
  TimeSteps=20,
  ResourceRegenerationPerTimeStep=1.15,
  harvestMax=25,
  ProbOfMobility=0.2){
  
  ## Set parameters
  PercWorking= 1-PercProtected #percent of resource in a working landscape
  TotalCCResourceProtected=PercProtected*TotalCarryingCapacity #total resource units in carrying capacity of protected area
  TotalCCResourceWorking=PercWorking*TotalCarryingCapacity #total resource units in carrying capacity of working area
  StartResourceWorking = TotalCCResourceWorking*StartPercCarryingCapacity #number of resources in the working landscape starting
  StartResourceProtected = TotalCCResourceProtected*StartPercCarryingCapacity #number of resources in the protected landscape starting
  CoopNumStart= as.integer(CoopPercStart*Individuals) #number of individuals cooperating fully at t0
  DefNumStart= Individuals - CoopNumStart#number of individuals defecting at t0
  PercTimeProtected = c(rep(0,CoopNumStart),rbeta(DefNumStart,1,2)) #percent of their foraging time each indv spends in the PA
  PercTimeWorking = (1-PercTimeProtected)#percent of their foraging time each indv spends in the working landscape
  
  agents<-data.frame(PercTimeProtected,
                     PercTimeWorking,
                     PayoffProtectedLastTime = rep(NA,Individuals),
                     PayoffWorkingLastTime= rep(NA,Individuals))   #dataframe to be filled with initial payoffs
  
  ProtectPerDefect<-ifelse(DefNumStart ==0,as.integer(StartResourceProtected),as.integer(StartResourceProtected/DefNumStart)) #protected area resources per each individual harvesting there
  WorkingPerTotal<-as.integer(StartResourceWorking/Individuals) #working landscape resources per individual
  
  for ( i in 1:nrow(agents)){  #fill the starting df percent payoff from the protected landscape is a function of the amount 
    agent2<-agents[i,]   #of resources there per individual searching there, and a random draw with a chance of success equal to time spent
    agents[i,4]<-rbinom(1,WorkingPerTotal,agent2$PercTimeWorking)
    agents[i,4]<-ifelse(agents[i,4]>=harvestMax,harvestMax,agents[i,4])
    agents[i,3]<- ifelse(agents[i,4]< harvestMax,rbinom(1,ProtectPerDefect,agent2$PercTimeProtected),0) #if they didnt get the max harvest, then 
    agents[i,3]<-ifelse(agents[i,3]>harvestMax-agents[i,4],harvestMax-agents[i,4],agents[i,3])
  } #see what they get from the working landscape first
  
  
  
  agents$PayoffTotalLastTime<- agents$PayoffProtectedLastTime + agents$PayoffWorkingLastTime #total payoff is the sum of what they get from both areas
  
  
  output<-data.frame(timeStep = 1:TimeSteps, #create df to fill with the outputs from each time step
                     meanTimeProtect = rep(NA,TimeSteps),
                     meanTimeWorking = rep(NA,TimeSteps),
                     NumResourcesProtect = rep(NA,TimeSteps),
                     percCCProtect = rep(NA,TimeSteps),
                     NumResourcesWorking = rep(NA,TimeSteps),
                     percCCWorking = rep(NA,TimeSteps),
                     meanPayoff = rep(NA,TimeSteps))
  
  output$meanTimeProtect[1] <- mean(agents$PercTimeProtected)  #fill it with the outouts from time 1
  output$meanTimeWorking[1]  <- mean(agents$PercTimeWorking) 
  output$NumResourcesProtect[1] <- StartResourceProtected-sum(agents$PayoffProtectedLastTime) 
  output$NumResourcesWorking[1] <- StartResourceWorking-sum(agents$PayoffWorkingLastTime) 
  output$percCCProtect[1]  <- output$NumResourcesProtect[1]/TotalCCResourceProtected
  output$percCCWorking[1]  <- output$NumResourcesWorking[1]/TotalCCResourceWorking
  output$meanPayoff[1]  <- mean(agents$PayoffTotalLastTime) 
  
  
  
  for (t in 2:TimeSteps){  #run the abm for all o fthe time steps
    
    #choose new strategy  
    
    LastTimeAgents<-agents  #save the agents from the pervious time to a new df
    
    LastTimePercTimeProtected<-LastTimeAgents$PercTimeProtected  #pull out the previous pretected foraging times as vector
    LastTimePercWorking<-LastTimeAgents$PercTimeWorking  #pull out the previous working foraging times as vector
    
    PercTimeProtected<-rep(NA,length(PercTimeProtected))  #nullify the previous perc time protected vector so we can fill it later and be confident the values changed
    
    if(LearningStrategy == "Success Bias"){ 
      for (j in 1:nrow(LastTimeAgents)){  
        ThisAgent<-LastTimeAgents[j,]      #modify the foraging strategy of each agent individually
        OtherAgents<-LastTimeAgents[-j,]   #dataframe of ther agents for them to copy from
        ThisAgentPayoff<-ThisAgent$PayoffTotalLastTime   #payoff last time period for this agent
        OtherAgentSpecific<-OtherAgents[sample(nrow(OtherAgents),1),] #choose a specific agent for them to get paired up with
        OtherAgentPayoff<-OtherAgentSpecific$PayoffTotalLastTime  #Payoff the other agent recieved
        OtherAgentPercProtect<-OtherAgentSpecific$PercTimeProtected #strategy of other agent
        ThisAgentPercProtect<-ThisAgent$PercTimeProtected  #strategy of this agent
        
        PercTimeProtected[j]<-ifelse(ThisAgentPayoff >=OtherAgentPayoff,ThisAgent$PercTimeProtected,
                                     (ThisAgentPercProtect+((OtherAgentPercProtect-ThisAgentPercProtect)/2))) #vector of new strategies
        
        #if this agent did better than the randomly selected agent then they keep their strategy
        #if they did worse thnan they modify their strategy to be the difference between their previou sstrategy and the randomly selected agent
        
      }}
    #if(LearningStrategy == "Conformist"){
     # for (j in 1:nrow(LastTimeAgents)){  
      #  ThisAgent<-LastTimeAgents[j,]      #Pull out each specific agent
       # OtherAgents<-LastTimeAgents[-j,]   #dataframe of ther agents for them to copy from
        #OtherAgentsSample<-OtherAgents[c(sample(1:nrow(OtherAgents),5)),] #haphazardly chose 5 here...look into lit to find good number!!!
        #OtherAgentsPercProtect<-mean(OtherAgentsSample$PercTimeProtected )# mean strategy of other agents
        #ThisAgentPercProtect<-ThisAgent$PercTimeProtected  #strategy of this agent
        
        #PercTimeProtected[j]<-(ThisAgentPercProtect+((OtherAgentsPercProtect-ThisAgentPercProtect)/2)) #vector of new strategies
      #}}
    
 #   if(LearningStrategy == "Conformist"){
  #    for (j in 1:nrow(LastTimeAgents)){  
   #     ThisAgent<-LastTimeAgents[j,]      #Pull out each specific agent
    #    OtherAgents<-LastTimeAgents[-j,]   #dataframe of ther agents for them to copy from
     #   OtherAgentsSample<-OtherAgents[c(sample(1:nrow(OtherAgents),5)),] #haphazardly chose 5 here...look into lit to find good number!!!
      #  ThisAgentPercProtect<-ThisAgent$PercTimeProtected
       # if(length(which(OtherAgentsSample$PercTimeProtected>0.0))>2 & ThisAgentPercProtect ==0.0){PercTimeProtected[j]<-rbeta(1,1,2)}
        #if(length(which(OtherAgentsSample$PercTimeProtected>0.0))>2 & ThisAgentPercProtect >0.0){PercTimeProtected[j]<-ThisAgentPercProtect}
    #    if(length(which(OtherAgentsSample$PercTimeProtected==0.0))>2){PercTimeProtected[j]<-0.0}
  #  }}
    
    if(LearningStrategy == "Conformist"){
      for (j in 1:nrow(LastTimeAgents)){  
        ThisAgent<-LastTimeAgents[j,]      #Pull out each specific agent
        OtherAgents<-LastTimeAgents[-j,]   #dataframe of ther agents for them to copy from
        OtherAgentsSample<-OtherAgents[c(sample(1:nrow(OtherAgents),5)),] #haphazardly chose 5 here...look into lit to find good number!!!
        ThisAgentPercProtect<-ThisAgent$PercTimeProtected
        if(length(which(OtherAgentsSample$PercTimeProtected>0.0))>2 ){
          PercTimeProtected[j]<-mean(OtherAgentsSample[OtherAgentsSample$PercTimeProtected!=0.0,]$PercTimeProtected )# mean strategy of other agents
         }
        if(length(which(OtherAgentsSample$PercTimeProtected==0.0))>2){PercTimeProtected[j]<-0.0}
      }}
    
    
    
    
    PercTimeWorking<- 1-PercTimeProtected   #time foraging in the working landscape
    
    
    agents<-data.frame(PercTimeProtected,   #new agent dataframe to fill
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
    
    
    ##calculate available resources per individual
    if(nrow(agents[agents$PercTimeProtected>0,])==0){ProtectPerDefect<-NewProtectedResourcesTotal}
    if(nrow(agents[agents$PercTimeProtected>0,])!=0){ProtectPerDefect<-as.integer(NewProtectedResourcesTotal/nrow(agents[agents$PercTimeProtected>0,]))}
    WorkingPerTotal<-as.integer(NewWorkingResourcesTotal/Individuals)
    
    
    for ( i in 1:nrow(agents)){  #fill the starting df percent payoff from the protected landscape is a function of the amount 
      agent2<-agents[i,]   #of resources there per individual searching there, and a random draw with a chance of success equal to time spent
      agents[i,4]<-rbinom(1,WorkingPerTotal,agent2$PercTimeWorking)
      agents[i,4]<-ifelse(agents[i,4]>=harvestMax,harvestMax,agents[i,4])
      agents[i,3]<- ifelse(agents[i,4]< harvestMax,rbinom(1,ProtectPerDefect,agent2$PercTimeProtected),0) #if they didnt get the max harvest, then 
      agents[i,3]<-ifelse(agents[i,3]>harvestMax-agents[i,4],harvestMax-agents[i,4],agents[i,3])
    } #see what they get from the working landscape first
    
    
    
    agents$PayoffTotalLastTime<- agents$PayoffProtectedLastTime + agents$PayoffWorkingLastTime #total payoff is the sum of what they get from both areas
    
    output$meanTimeProtect[t] <- mean(agents$PercTimeProtected)  #fill it with the outouts from time 1
    output$meanTimeWorking[t]  <- mean(agents$PercTimeWorking) 
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
    geom_line(aes(y=meanTimeWorking),size=3,color="#756bb1")+
    theme_classic()+ylab("Mean Percent Time in Working landscape")+
    ggtitle("Time allocation (Cooperation vs Defection)")
  
  x<-gridExtra::grid.arrange(p1,p2,p3,ncol=1)
  
  
  return(output) 
}

abm(Individuals=100, #number of total resource users in a population
    
    TotalCarryingCapacity=10000, #total available resource units
    
    StartPercCarryingCapacity = 0.65, #amount of resources available in the landscape at the start in proportion to CC
    
    PercProtected=0.8, #percent of the total resource that's in a protected area
    
    CoopPercStart=1.0, #percent of individuals who start by following the rules at t0
    
    LearningStrategy = "Success Bias", #options are Success Bias & Conformist
    
    TimeSteps=30,
    ResourceRegenerationPerTimeStep=1.5,
    harvestMax=22,
    ProbOfMobility=0.99)

abm(LearningStrategy = "Conformist",CoopPercStart=0.5)

