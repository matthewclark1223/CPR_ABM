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

samples<-seq(from=10,to=20, by=5)

CORS<-rep(NA,length=length(samples))
pwrRES<-data.frame(samples=samples,CORS=CORS)

for(i in 1:length(samples) ){
  






sample_days<-samples[i] #from samples
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


posts <- rstan::extract(mod)



# Model predictions across the sampling time period.
# These were generated with the "fake" data and time series.
mod_median = apply(posts$fake_I[,,2], 2, median)


COR<-cor(mod_median,na.omit(SucBiasdat[]$Enrolled)/100)

pwrRES[i,2]<-COR
}
