#cows stan minimal example, bayesian approach to fitting wilmink curves
#pkgbuild::has_build_tools(debug = TRUE)
# Sys.setenv(RTOOLS40_HOME = "C:/rtools40")
# Sys.getenv()
#install.packages(c("Rcpp", "RcppEigen", "rstan"), type = "source")

library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#Sys.setenv(LOCAL_CPPFLAGS = '-march=native')

y <- as.matrix(read.table('cows.txt', header = TRUE))
x <- seq(7,by=7,length.out=35)
z <- as.vector(read.table('herd_total.txt', header = TRUE)) 
z <- as.numeric(z[,1])
w <- as.matrix(read.table('herd_to_cow.txt', header = TRUE)) 
v <- as.vector(read.table('cow_total.txt', header = TRUE)) 
v <- as.numeric(v[,1])


N <- nrow(y)
T <- ncol(y)
H <- length(z)

#example(stan_model, package = "rstan", run.dontrun = TRUE)
# Sys.getenv("BINPREF") 
# Sys.which("g++")

#run stan
cows_fit <- stan(file = 'cows.stan', data=c("N","T","H","x","y","z","w","v")) #takes about 20 seconds with no errors for individual fit, 60s with no rds file
print(cows_fit)

#save file
cows_fit@stanmodel@dso <- new("cxxdso") 
#saveRDS(cows_fit, file = "fitwithonlycumulative.rds")
#saveRDS(cows_fit, file = "fitwithonlydailydata.rds")

