


library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("AbmOutput"),

    # Sidebar with a slider input for number of bins     
    sidebarLayout(
        sidebarPanel(
            sliderInput("individuals",
                        "Number of individuals:",
                        min = 10,
                        max = 1000,
                        value = 100),
           sliderInput("totResources","Total Resources Max",
                       min = 100,max=100000,value=10000),
           sliderInput("timeSteps",
                       "Time Steps",
                       min = 10,
                       max = 1000,
                       value = 100),
           sliderInput("percProtect",
                       "Percent of Resources Protected",
                       min = 0.01,
                       max = 0.99,
                       value = 0.20),
           sliderInput("startCooperators",
                       "Starting Obligatory Cooperators",
                       min = 0.1,
                       max = 0.95,
                       value = 0.9),
           sliderInput("resourceRegen",
                       "Resource Regeneration Rate",
                       min = 1.1,
                       max = 1.9,
                       value = 1.5),
           sliderInput("maxHarvest",
                       "Max individual harvest per time step",
                       min = 10,
                       max = 100,
                       value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    abm<-function(#Specified parameters
        Individuals=100, #number of total resource users in a population
        
        TotalResourceUnit=10000, #total available resource units
        
        PercProtected=0.3, #percent of the total resource that's in a protected area
        
        CoopPercStart=0.9, #percent of individuals who start by following the rules at t0
        
        TimeSteps=20,
        ResourceRegenerationPerTimeStep=1.5,
        harvestMax=25){
        
        ## Set parameters
        PercWorking= 1-PercProtected #percent of resource in a working landscape
        TotalResourceWorking = PercWorking*TotalResourceUnit #number of resources in the working landscape starting
        TotalResourceProtected = PercProtected*TotalResourceUnit #number of resources in the protected landscape starting
        CoopNumStart= CoopPercStart*Individuals #number of individuals cooperating fully at t0
        
        DefNumStart= (1-CoopPercStart)*Individuals #number of individuals defecting at t0
        PercTimeProtected = c(rep(0,CoopNumStart),rbeta(DefNumStart+1,1,2)) #percent of their foraging time each indv spends in the PA
        PercTimeWorking = (1-PercTimeProtected)#percent of their foraging time each indv spends in the working landscape
        
        agents<-data.frame(PercTimeProtected,
                           PercTimeWorking,
                           PayoffProtectedLastTime = rep(NA,Individuals),
                           PayoffWorkingLastTime= rep(NA,Individuals))   #dataframe to be filled with initial payoffs
        
        ProtectPerDefect<-as.integer(TotalResourceProtected/DefNumStart) #protected area resources per each individual harvesting there
        WorkingPerTotal<-as.integer(TotalResourceWorking/Individuals) #working landscape resources per individual
        
        for ( i in 1:nrow(agents)){  #fill the starting df percent payoff from the protected landscape is a function of the amount 
            agent2<-agents[i,]   #of resources there per individual searching there, and a random draw with a chance of success equal to time spent
            agents[i,4]<-rbinom(1,WorkingPerTotal,agent2$PercTimeWorking)
            agents[i,4]<-ifelse(agents[i,4]>harvestMax,harvestMax,agents[i,4])
            agents[i,3]<- ifelse(agents[i,4]<= harvestMax,rbinom(1,ProtectPerDefect,agent2$PercTimeProtected),0) #if they didnt get the max harvest, then 
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
        output$NumResourcesProtect[1] <- TotalResourceProtected-sum(agents$PayoffProtectedLastTime) 
        output$NumResourcesWorking[1] <- TotalResourceWorking-sum(agents$PayoffWorkingLastTime) 
        output$percCCProtect[1]  <- output$NumResourcesProtect[1]/TotalResourceProtected
        output$percCCWorking[1]  <- output$NumResourcesWorking[1]/TotalResourceWorking
        output$meanPayoff[1]  <- mean(agents$PayoffTotalLastTime) 
        
        
        
        for (t in 2:TimeSteps){  #run the abm for all o fthe time steps
            
            #choose new strategy  
            
            LastTimeAgents<-agents  #save the agents from the pervious time to a new df
            
            LastTimePercTimeProtected<-LastTimeAgents$PercTimeProtected  #pull out the previous pretected foraging times as vector
            LastTimePercWorking<-LastTimeAgents$PercTimeWorking  #pull out the previous working foraging times as vector
            
            PercTimeProtected<-rep(NA,length(PercTimeProtected))  #nullify the previous perc time protected vector so we can fill it later and be confident the values changed
            for (j in 1:nrow(LastTimeAgents)){  
                ThisAgent<-LastTimeAgents[j,]      #modify the foraging strategy of each agent individualls
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
                
            }
            
            PercTimeWorking<- 1-PercTimeProtected   #time foraging in the working landscape
            
            
            agents<-data.frame(PercTimeProtected,   #new agent dataframe to fill
                               PercTimeWorking,
                               PayoffProtectedLastTime = rep(NA,Individuals),
                               PayoffWorkingLastTime= rep(NA,Individuals)) 
            
            
            #New resource pools to pull from
            #make sure they dont regenerate past their carrying capacity
            
            NewProtectedResourcesTotal<-output$NumResourcesProtect[t-1] * ResourceRegenerationPerTimeStep
            NewProtectedResourcesTotal<-ifelse(NewProtectedResourcesTotal<=  TotalResourceProtected,NewProtectedResourcesTotal, TotalResourceProtected)
            NewWorkingResourcesTotal<-output$NumResourcesWorking[t-1] * ResourceRegenerationPerTimeStep
            NewWorkingResourcesTotal<-ifelse(NewWorkingResourcesTotal<=TotalResourceWorking,NewWorkingResourcesTotal,TotalResourceWorking)
            
            ProtectPerDefect<-as.integer(NewProtectedResourcesTotal/nrow(agents[agents$PercTimeProtected>0,])) #protected area resources per each individual harvesting there  
            WorkingPerTotal<-as.integer(NewWorkingResourcesTotal/Individuals)
            
            
            for ( i in 1:nrow(agents)){  #fill the starting df percent payoff from the protected landscape is a function of the amount 
                agent2<-agents[i,]   #of resources there per individual searching there, and a random draw with a chance of success equal to time spent
                agents[i,4]<-rbinom(1,WorkingPerTotal,agent2$PercTimeWorking)
                agents[i,4]<-ifelse(agents[i,4]>harvestMax,harvestMax,agents[i,4])
                agents[i,3]<- ifelse(agents[i,4]<= harvestMax,rbinom(1,ProtectPerDefect,agent2$PercTimeProtected),0) #if they didnt get the max harvest, then 
                agents[i,3]<-ifelse(agents[i,3]>harvestMax-agents[i,4],harvestMax-agents[i,4],agents[i,3])
            } #see what they get from the working landscape first
            
            
            
            agents$PayoffTotalLastTime<- agents$PayoffProtectedLastTime + agents$PayoffWorkingLastTime #total payoff is the sum of what they get from both areas
            
            output$meanTimeProtect[t] <- mean(agents$PercTimeProtected)  #fill it with the outouts from time 1
            output$meanTimeWorking[t]  <- mean(agents$PercTimeWorking) 
            output$NumResourcesProtect[t] <- NewProtectedResourcesTotal-sum(agents$PayoffProtectedLastTime) 
            output$NumResourcesWorking[t]  <- NewWorkingResourcesTotal-sum(agents$PayoffWorkingLastTime)
            output$percCCProtect[t]  <- output$NumResourcesProtect[t]/TotalResourceProtected
            output$percCCWorking[t]  <- output$NumResourcesWorking[t]/TotalResourceWorking
            output$meanPayoff[t]  <- mean(agents$PayoffTotalLastTime)
            
        }
        
        par(mfrow=c(3,1))
        plot(output$percCCProtect, type = 'l',col="green")
        plot(output$percCCWorking, type = 'l',col="brown")
        plot(output$meanPayoff, type = 'l',col="red")
        mtext("Output", side=3, outer=TRUE, line=-3,cex=0.8)
        
        
        
    }
    
    
    
    
    
    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
       individuals<-input$individuals
       totRecources<-input$totResources
       timeSteps<-input$timeSteps
       percProtect<-input$percProtect
       startCooperators<-input$startCooperators
       resourceRegen<-input$resourceRegen
       maxHarvest<-input$maxHarvest

        abm(Individuals = individuals, TotalResourceUnit =totRecources, 
            PercProtected=percProtect, CoopPercStart=startCooperators,
            ResourceRegenerationPerTimeStep=resourceRegen,harvestMax= maxHarvest,TimeSteps=timeSteps )
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
