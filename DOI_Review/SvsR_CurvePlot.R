library(deSolve)

InfRt<-seq(from=0.1, to=0.9, by =0.1 )

df<-data.frame(time=NA,S=NA,I=NA,R=NA,rate=NA)

for( b in 1:length(InfRt) ){
  
  I0 = 0.02    # initial fraction infected
  S0 = 1 - I0 # initial fraction susceptible
  R0 = 0
  
  # Assign transmission and pathogen-induced death/recovery rates:
  beta = InfRt[b] #rate of infection
  gamma = 0.00 #rate of recovery
  
  # We will use the package deSolve to integrate, which requires certain data structures.
  # Store parameters and initial values
  # Parameters must be stored in a named list.
  params <- list(beta = beta,
                 gamma = gamma)
  
  # Initial conditions are stored in a vector
  inits <- c(S0, I0, R0) #0 denotes that it is an initial condition
  
  # Create a time series over which to integrate.
  # Here we have an epidemic that is observed over t_max number of days (or weeks or etc).
  t_min = 0
  t_max = 100
  times = t_min:t_max
  
  # We must create a function for the system of ODEs.
  # See the 'ode' function documentation for further insights.
  SIR <- function(t, y, params) {
    with(as.list(c(params, y)), {
      
      dS = - beta * y[1] * y[2] #y[1] susceptible and y[2] is infected
      
      dI = beta * y[1] * y[2] - gamma * y[2] #susceptibles and recovered
      
      dR = gamma * y[2] #rate of recovery and how many infected there are
      
      res <- c(dS,dI,dR)
      list(res)
    })
  }
  
  # Run the integration:
  out <- ode(inits, times, SIR, params, method="rk")
  # Store the output in a data frame:
  out <- data.frame(out)
  colnames(out) <- c("time", "S", "I", "R")
  out$rate<-rep(InfRt[b],nrow(out))
  
  df<-rbind(df,out)
}

#library(tidyverse)
mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                plot.title = element_text( size=18, color="black",face="bold"),
                axis.title.x = element_text( size=18),
                axis.title.y = element_text( size=18),
                axis.text=(element_text(color="black", size=14)))


ggplot(df,aes(x=time,y=I))+geom_line(aes(by=as.character(rate),color=rate),size=1.5,alpha=0.9)+ 
  scale_colour_gradient(low = "#3690c0",high = "black")+theme_classic()+
  annotate( "text", label = "S curve",x = 65, y = 0.85, size = 8, colour = "#3690c0")+
  annotate( "text", label = "R curve",x = 2, y = 0.93, size = 8, colour = "black")+
  theme(legend.position = "none")+xlab("Time")+ylab("Proportion Adopted")+mytheme
#


#More accurate but less aesthetically pleasing example

df2<-df%>%filter(rate==0.1)
z<-data.frame(time=0:100,s=rep(NA,101),I=ifelse((log(0:100)/4.5)>1,1,(log(0:100)/4.5)),R=rep(NA,101),rate=rep(NA,101))


ggplot(df2,aes(x=time,y=I))+geom_line(color="#3690c0",size=1.5,alpha=0.99)+ 
  geom_line(data=z,aes(x=time,y=I),color="black",size=1.5,alpha=0.99,linetype=2)+
  theme_classic()+
  annotate( "text", label = "S curve",x = 45, y = 0.52, size = 8, colour = "#3690c0")+
  annotate( "text", label = "R curve",x = 20, y = 0.75, size = 8, colour = "black")+
  theme(legend.position = "none")+xlab("Time")+ylab("Proportion Adopted")+mytheme+
  scale_x_continuous(expand = c(0, 2)) + scale_y_continuous(expand = c(0, 0))




