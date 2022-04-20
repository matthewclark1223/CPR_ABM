# Load some required packages
##############
library(deSolve)
library(dplyr)
library(ggplot2)
library(rstan)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())
##############

I0 = 0.02    # initial fraction infected
S0 = 1 - I0 # initial fraction susceptible
R0 = 0 #initial fraction recovered
Pr0 = 0.2 #starting protected resources cc
Up0 = 0.2 #starting unprotected CC

# Assign transmission and pathogen-induced death/recovery rates:
beta = 0.60 #rate of infection
gamma = 0.10 #rate of recovery
chi = 0.05 # rate at which recovered individuals are added back to the susceptible group CHANGE to 1 tp get main dynamic of interest!!
alpha = 0.009 #stregnth of resource effects on adoption, 
epsilon = 0.002 #stregnth of harvest on resources..Maybe actually harvest/person??
lambda=0.008 #regrowth of resources I think
tao= 0.001 #stregnth of resource level on nonadoption? maybe redundant w/alpha
theta=0.002 #strength of harvest on unprotected
omega= 0.008 #regrowth of resources, maybe redundant with lambda

# We will use the package deSolve to integrate, which requires certain data structures.
# Store parameters and initial values
# Parameters must be stored in a named list.
params <- list(beta = beta,
               gamma = gamma, 
               chi = chi,
               alpha=alpha,
               epsilon=epsilon,
               lambda=lambda,
               tao=tao,
               theta=theta,
               omega=omega)

# Initial conditions are stored in a vector
inits <- c(S0, I0, R0,Pr0,Up0) #0 denotes that it is an initial condition

# Create a time series over which to integrate.
# Here we have an epidemic that is observed over t_max number of days (or weeks or etc).
t_min = 0
t_max = 50
times = t_min:t_max

# We must create a function for the system of ODEs.
# See the 'ode' function documentation for further insights.
SIRS <- function(t, y, params) {
  with(as.list(c(params, y)), {
    
    dS = - beta * y[1] * y[2] + chi*y[3]    #y[1] susceptible and y[2] is infected and y[3] is recovered y[4] is protected resources
    
    dI = beta * y[1] * y[2] - gamma * y[2] -alpha* y[4] +tao*y[5]  #susceptibles and recovered
    
    dR = gamma * y[2] -chi*y[3]  + alpha*y[4] -tao*y[5]   #rate of recovery and how many infected there are
    
    dPr = -epsilon*(y[1]+y[3])+lambda*y[4] #rate of change in protected area
    
    dUp = -theta * (y[1]+y[2]+y[3])+ omega*y[5] #rate of change in Unprotected
    
    res <- c(dS,dI,dR,dPr,dUp)
    list(res)
  })
}

# Run the integration:
out <- ode(inits, times,SIRS, params, method="rk")
#
# Store the output in a data frame:
out <- data.frame(out)
colnames(out) <- c("time", "S", "I", "R","Pr","Up")

# quick plot of the epidemic
plot(NA,NA, xlim = c(t_min, t_max), xlab = "Time",ylim=c(0,1), ylab="Fraction of Host Population")
lines(out$S+out$R ~ out$time, col="black")
lines(out$I ~ out$time, col="red")
lines(out$Pr~out$time, col="green")
lines(out$Up~out$time, col="blue")
legend(x = 30, y = 0.8, legend = c("Not Enrolled", "Enrolled","PA Resources", "Up Resources"), 
       col = c("black", "red","green","blue"), lty = c(1, 1), bty="n")



####  needs to be modified below######################
sample_days = 50 # number of days sampled throughout the epidemic. changing this around
sample_n = 100 # number of host individuals sampled per day

# Choose which days the samples were taken. 
# Ideally this would be daily, but we all know that is difficult.
#sample_time = sort(sample(1:t_max, sample_days, replace=F))
sample_time = 1:sample_days #just sample the beginning!

# Extract the "true" fraction of the population that is infected on each of the sampled days:
sample_propinf = out[out$time %in% sample_time, 3]
sample_propinf_Pr = out[out$time %in% sample_time, 5]
sample_propinf_UpR = out[out$time %in% sample_time, 6]

# Generate binomially distributed data.
# So, on each day we sample a given number of people (sample_n), and measure how many are infected.
# We expect binomially distributed error in this estimate, hence the random number generation.
PCC<-1000*0.2
UpCC<-1000*0.8
sample_y = rbinom(sample_days, sample_n, sample_propinf)
sample_Pr = rbinom(sample_days, PCC, sample_propinf_Pr)
sample_Up = rbinom(sample_days, UpCC, sample_propinf_UpR)



###fitting Stan data

stan_d = list(n_obs = sample_days,
              n_params = length(params),
              n_difeq = length(inits),
              n_sample = sample_n,
              n_fake = length(1:t_max),
              y = sample_y,
              PaR = sample_Pr/PCC,
              UpR = sample_Up/UpCC,
              PCC = PCC,
              UpCC = UpCC,
              S0 = 0.98, #starting susceptible 
              PAR0 = 0.2, #starting CC 
              Up0 = 0.2, #starting CC 
              t0 = 0,
              ts = sample_time,
              fake_ts = c(1:t_max))

# Which parameters to monitor in the model:
params_monitor = c("y_hat", "y0", "params", "fake_I") #fake_I from generated quantities

# Fit and sample from the posterior
mod = stan("~/Pemba_Project/DOI_Review/SIRSplusResourcelvl.stan",
           data = stan_d,
           pars = params_monitor,
           chains = 1,
           warmup = 500,
           iter = 1500)

# You should do some MCMC diagnostics, including:
traceplot(mod, pars="lp__")

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

ggplot(df_sample, aes(x=sample_time, y=sample_prop)) +
  geom_point(col="black", shape = 19, size = 1.5) +
  # Error in integration:
  geom_line(data = df_fit, aes(x=mod_time, y=mod_median), color = "red") + 
  geom_line(data = df_fit, aes(x=mod_time, y=mod_high), color = "red", linetype=3) + 
  geom_line(data = df_fit, aes(x=mod_time, y=mod_low), color = "red", linetype=3) + 
  # Aesthetics
  labs(x = "Time (days)", y = "Proportion Infected") + 
  scale_x_continuous(limits=c(0, 50), breaks=c(0,25,50)) +
  scale_y_continuous(limits=c(0,1), breaks=c(0,.5,1)) +
  theme_classic() + 
  theme(axis.line.x = element_line(color="black"),
        axis.line.y = element_line(color="black"))
