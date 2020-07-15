
eta <- c(0.05, 0.05)
gamma <- c(1.8, 0.4)
N <- 200
x <- rpois(N, 8)
z <- rnorm(N, 0, 2)
mu <- binomial(link = logit)$linkinv(eta[1] + eta[2]*x)
phi <- binomial(link = log)$linkinv(gamma[1] + gamma[2]*z)
y <- rbeta(N, mu * phi, (1 - mu) * phi)
dat <- data.frame(cbind(y, x, z))
plot(dat$x,dat$y)
hist(dat$y, col = "darkgrey", border = F, main = "Distribution of Outcome Variable", xlab = "y", breaks = 20, freq = F)
fit1 <- stan_betareg(y ~ x | z, data = dat, link = "logit", link.phi = "log",
                     cores = 3, chains=3)

round(coef(fit1),2)
bayesplot::bayesplot_grid(pp_check(fit1))



eta <- c(0.05, 0.05)
N <- 200
x <- rpois(N, 8)
z <- rnorm(N, 0, 2)
mu <- binomial(link = logit)$linkinv(eta[1] + eta[2]*x)
phi <- 2
y <- rbeta(N, mu * phi, (1 - mu) * phi)
dat <- data.frame(cbind(y, x, z))
plot(dat$x,dat$y)
hist(dat$y, col = "darkgrey", border = F, main = "Distribution of Outcome Variable", xlab = "y", breaks = 20, freq = F)
fit1 <- stan_betareg(y ~ x | z, data = dat, link = "logit", link.phi = "log",
                     cores = 3, chains=3)

round(coef(fit1),2)
