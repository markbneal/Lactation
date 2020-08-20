#test stan installation with rats example
#from https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

#model <- stan_demo()
#278 is rats

library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

y <- as.matrix(read.table('rats.txt', header = TRUE))
x <- c(8, 15, 22, 29, 36)
xbar <- mean(x)
N <- nrow(y)
T <- ncol(y)
rats_fit <- stan('rats.stan', data=c("N","T","x","y","xbar"))
print(rats_fit)

