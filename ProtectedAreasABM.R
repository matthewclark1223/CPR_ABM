

abm<-function(#Specified parameters
  Individuals=100, #number of total resource users in a population
  
  TotalResourceUnit=10000, #total available resource units
  
  PercProtected=0.2, #percent of the total resource that's in a protected area
  
  CoopPercStart=0.9, #percent of individuals who start by following the rules at t0
  
  TimeSteps=200){
  
  ## Set parameters
  PercWorking= 1-PercProtected #percent of resource in a working landscape
  TotalResourceWorking = PercWorking*TotalResourceUnit #number of resources in the working landscape starting
  TotalResourceProtected = PercProtected*TotalResourceUnit #number of resources in the protected landscape starting
  CoopNumStart= CoopPercStart*Individuals #number of individuals cooperating fully at t0
  
  DefNumStart<- (1-CoopPercStart)*Individuals #number of individuals defecting at t0

  PercTimeProtected = c(rep(0,CoopNumStart),rbeta(DefNumStart+1,1,2))
  PercTimeWorking = (1-PercTimeProtected)
  
  agents<-data.frame(PercTimeProtected,
                     PercTimeWorking,
                     PayoffProtectedLastTime = rep(NA,Individuals),
                     PayoffWorkingLastTime= rep(NA,Individuals))
  
  for ( i in 1:nrow(agents)){
    agent2<-agents[i,]
    agents[i,3]<- rbinom(1,TotalResourceProtected/DefNumStart,agent2$PercTimeProtected)
    agents[i,4]<-rbinom(1,TotalResourceWorking/Individuals,agent2$PercTimeWorking)}
  
  agents$PayoffTotalLastTime<- agents$PayoffProtectedLastTime + agents$PayoffWorkingLastTime
   
  
  
  #this is currently broken because the protected area followers will always beat out the defecters
  # Maybe this will be ok? 
  #might change iver time
  #think through how the resource extraction effects each pool 
  
  return(agents)
  
  
}
abm()








