#cows stan minimal example, bayesian approach to fitting wilmink curves
#setwd("C:/Users/nealm/Desktop/dairynz/Lactation")
library("rstan")
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

y <- as.matrix(read.table('cows.txt', header = TRUE))
x <- seq(7,by=7,length.out=35)
z <- as.vector(read.table('herd_total.txt', header = TRUE)) 
z <- as.numeric(z[,1])
w <- as.matrix(read.table('herd_to_cow.txt', header = TRUE)) 
v <- as.vector(read.table('cow_total.txt', header = TRUE)) 
v <- as.numeric(v[,1])

class(x)
class(z)
N <- nrow(y)
T <- ncol(y)
H <- length(z)
#rstan_options(auto_write = TRUE)
cows_fit <- stan(file = 'cows.stan') #takes about 20 seconds with no errors, 60s with no rds file
print(cows_fit)
#save file
cows_fit@stanmodel@dso <- new("cxxdso") 
#saveRDS(cows_fit, file = "fitwithonlycumulative.rds")
saveRDS(cows_fit, file = "fitwithonlydailydata.rds")
