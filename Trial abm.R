abmtrial <- function (N, t_max,resourceOpen,resourceProtected) {
  
  agent<-data.frame(behavior = rep("C",N),stringsAsFactors = F)
  
  
  
  output <- data.frame(p = rep(NA, t_max))
  
  output$p[1] <- sum(agent$trait == "C") / N
  
  for (t in 2:t_max) {
    
    previous_agent <- agent  # copy agent to previous_agent dataframe
    
    prev_resourceOpen <- resourceOpen
    prev_resourceProtected <-resourceProtected
    
    resourceOpen <- (resourceOpen - nrow(agent$behavior == "C"))
    resourceProtected <- (resourceProtected - nrow(agent$behavior == "D"))
    
    probCoop <- 0.5
    
    agent <- data.frame(behavior = sample(c("C","D"), N, replace = TRUE,
                                          prob=c(probCoop,1-probCoop)),stringsAsFactors = "F")  # randomly copy from previous generation
    
    output$p[t] <- sum(agent$behavior == "C") / N  # get p and put it into output slot for this generation t
    
  }
  
 # plot(output$p, type = 'l', ylab = "p, proportion of agents cooperating", xlab = "generation", ylim = c(0,1), main = paste("N =", N))
  plot(output$resourceOpen, type = 'l', ylab = "Amount of common pool resource", xlab = "generation", main = paste("N =", N))
  
}

abmtrial(55,25,200,20)
