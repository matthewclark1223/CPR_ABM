#from Smaldino & Jones 2020
#We model behavior adoption as a susceptible-infectious-susceptible (SIS) process, in
#120 which individuals can oscillate between adoption and non-adoption of the behavior indefinitely. We view this as more realistic than an SIR process for preventative-but-transient
#behaviors like social distancing or wearing face masks

#SIRS equation from https://docs.idmod.org/projects/emod-generic/en/latest/model-sir.html
#stan model relies heavily on https://jrmihalj.github.io/estimating-transmission-by-fitting-mechanistic-models-in-Stan/

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
R0 = 0

# Assign transmission and pathogen-induced death/recovery rates:
beta = 0.60 #rate of infection
gamma = 0.10 #rate of recovery
chi = 0.05 # rate at which recovered individuals are added back to the susceptible group CHANGE to 1 tp get main dynamic of interest!!

# We will use the package deSolve to integrate, which requires certain data structures.
# Store parameters and initial values
# Parameters must be stored in a named list.
params <- list(beta = beta,
               gamma = gamma, 
               chi = chi)

# Initial conditions are stored in a vector
inits <- c(S0, I0, R0) #0 denotes that it is an initial condition

# Create a time series over which to integrate.
# Here we have an epidemic that is observed over t_max number of days (or weeks or etc).
t_min = 0
t_max = 50
times = t_min:t_max

# We must create a function for the system of ODEs.
# See the 'ode' function documentation for further insights.
SIRS <- function(t, y, params) {
  with(as.list(c(params, y)), {
    
    dS = - beta * y[1] * y[2] + chi*y[3]#y[1] susceptible and y[2] is infected and y[3] is recovered
    
    dI = beta * y[1] * y[2] - gamma * y[2] #susceptibles and recovered
    
    dR = gamma * y[2] -chi*y[3]#rate of recovery and how many infected there are
    
    res <- c(dS,dI,dR)
    list(res)
  })
}

# Run the integration:
out <- ode(inits, times, SIRS, params, method="rk")
#
# Store the output in a data frame:
out <- data.frame(out)
colnames(out) <- c("time", "S", "I", "R")

# quick plot of the epidemic
plot(NA,NA, xlim = c(t_min, t_max), ylim=c(0, 1), xlab = "Time", ylab="Fraction of Host Population")
lines(out$S ~ out$time, col="black")
lines(out$I ~ out$time, col="red")
#lines(out$R~out$time, col="green")
legend(x = 30, y = 0.8, legend = c("Susceptible", "Infected"), 
       col = c("black", "red"), lty = c(1, 1), bty="n")


sample_days = 20 # number of days sampled throughout the epidemic. changing this around
sample_n = 100 # number of host individuals sampled per day

# Choose which days the samples were taken. 
# Ideally this would be daily, but we all know that is difficult.
#sample_time = sort(sample(1:t_max, sample_days, replace=F))
sample_time = 1:sample_days #just sample the beginning!

# Extract the "true" fraction of the population that is infected on each of the sampled days:
sample_propinf = out[out$time %in% sample_time, 3]

# Generate binomially distributed data.
# So, on each day we sample a given number of people (sample_n), and measure how many are infected.
# We expect binomially distributed error in this estimate, hence the random number generation.
sample_y = rbinom(sample_days, sample_n, sample_propinf)
#lines(out$I*25 ~ out$time, col="red") #deterministic


###fitting Stan data

stan_d = list(n_obs = sample_days,
              n_params = length(params),
              n_difeq = length(inits),
              n_sample = sample_n,
              n_fake = length(1:t_max),
              y = sample_y,
              t0 = 0,
              ts = sample_time,
              fake_ts = c(1:t_max))

# Which parameters to monitor in the model:
params_monitor = c("y_hat", "y0", "params", "fake_I") #fake_I from generated quantities

# Fit and sample from the posterior
mod = stan("~/Pemba_Project/DOI_Review/SIRS.stan",
           data = stan_d,
           pars = params_monitor,
           chains = 3,
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
