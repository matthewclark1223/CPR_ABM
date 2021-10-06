source("Binary_Adoption_ABM.R")

# Load some required packages
##############
library(deSolve)
library(dplyr)
library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
##############

###fitting Stan data

sample_days<-30#30
sample_n<-100
t_max<-100
sample_y<-deqdatPOS_Conform[1:sample_days,]$Enrolled
sample_time<-1:sample_days
# Parameters must be stored in a named list.
params <- list(beta = NA, #rate of transmission
               gamma = NA, #rate of dropout
               chi = NA) #time after dropout before willing to try again

stan_d = list(n_obs = sample_days,
              n_params = length(params),
              n_difeq = 3, #there's 3 dif eqs
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
           chains = 1,
           warmup = 500,
           iter = 1500)


# You should do some MCMC diagnostics, including:
#traceplot(mod, pars="lp__")

traceplot(mod, pars=c("params", "y0"))
#summary(mod)$summary[,"Rhat"]

# These all check out for my model, so I'll move on.

# Extract the posterior samples to a structured list:
posts <- extract(mod)
hist(posts$params[,1])
hist(posts$params[,2])

#R0 number of infected people
R0<-(posts$params[,1]*0.20)/posts$params[,2]

length(which(R0<1))/length(R0)

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




# Plot the synthetic data with the model predictions
# Median and 95% Credible Interval

mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                plot.title = element_text( size=18, color="black",face="bold"),
                axis.title.x = element_text( size=18),
                axis.title.y = element_text( size=18),
                axis.text=(element_text(color="black", size=14)),
                legend.title = element_text(colour="black", size=18),
                legend.text = element_text( size = 14))


ggplot(df_sample, aes(x=sample_time, y=sample_prop)) +
  #points
  geom_point(aes(fill="Training"),col="black" ,shape = 21, size = 5,stroke=2) +
  geom_point(data=deqdatPOS_Conform[sample_days+1:sample_n,],aes(x=timeStep,y=Enrolled/100,fill="Test"),
             col="black" ,shape = 21, size = 5,stroke=2)+
  
  # Error in integration:
  geom_line(data = df_fit, aes(x=mod_time, y=mod_median), color = "black",alpha=0.5,size=1.5) + 
  geom_line(data = df_fit, aes(x=mod_time, y=mod_high), color = "black",alpha=0.5, linetype="longdash",size=1) + 
  geom_line(data = df_fit, aes(x=mod_time, y=mod_low), color = "black", alpha=0.5,linetype="longdash",size=1) + 
  
 
  # Aesthetics
  labs(x = "Time", y = "Proportion Enrolled") + 
  #scale_x_continuous(limits=c(0, 50), breaks=c(0,25,50)) +
  #scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1)) +
  
  #legend
  scale_fill_manual(name="",values=c("Training"="darkgrey","Test"="#3690c0"),
                    guide = guide_legend(reverse = TRUE))+

  theme_classic() + 
  theme(axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))+mytheme



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


# You should do some MCMC diagnostics, including:
traceplot(mod, pars="lp__")
names(mod)[94:96]<-c("Beta","Gamma","Chi")

traceplot(mod, pars=c("params"))+mytheme+theme(strip.text.x = element_text(size = 15))
#summary(mod)$summary[,"Rhat"]

# These all check out for my model, so I'll move on.

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

draws<-as_tibble(posts$fake_I[,,2])%>%add_column(draw=1:1000)
names(draws)[1:100]<-1:100

draws <-  pivot_longer(draws, c(1:100) , names_to = "mod_time")


# Plot the synthetic data with the model predictions
# Median and 95% Credible Interval

mytheme<- theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                plot.title = element_text( size=18, color="black",face="bold"),
                axis.title.x = element_text( size=18),
                axis.title.y = element_text( size=18),
                axis.text=(element_text(color="black", size=14)),
                legend.title = element_text(colour="black", size=18),
                legend.text = element_text( size = 14))


#cor(mod_median[-(1:sample_days)],(SucBiasdat[(sample_days+1):sample_n,]$Enrolled)/sample_n)

ggplot(df_sample, aes(x=sample_time, y=sample_prop)) +
  #samples
  geom_line(data=draws,mapping = aes(x = as.integer(mod_time), y=value, group = draw), alpha = 0.05, size=0.1) +

  #points
  geom_point(aes(fill="Training"),col="black" ,shape = 21, size = 5,stroke=2) +
  geom_point(data=SucBiasdat[sample_days+1:sample_n,],aes(x=timeStep,y=Enrolled/100,fill="Test"),
             col="black" ,shape = 21, size = 5,stroke=2)+
  
  # Error in integration:
  #geom_line(data = df_fit, aes(x=mod_time, y=mod_median), color = "black",alpha=0.5,size=1.5) + 
  #geom_line(data = df_fit, aes(x=mod_time, y=mod_high), color = "black",alpha=0.5, linetype="longdash",size=1) + 
  #geom_line(data = df_fit, aes(x=mod_time, y=mod_low), color = "black", alpha=0.5,linetype="longdash",size=1) + 
  
  
  
  
  # Aesthetics
  labs(x = "Time", y = "Proportion Enrolled") + 
  #scale_x_continuous(limits=c(0, 50), breaks=c(0,25,50)) +
  scale_y_continuous(limits=c(0,1)) +
  
  #legend
  scale_fill_manual(name="",values=c("Training"="darkgrey","Test"="#3690c0"),
                    guide = guide_legend(reverse = TRUE))+
  
  theme_classic() + 
  theme(axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))+mytheme



  
  






