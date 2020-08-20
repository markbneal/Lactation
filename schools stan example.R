##Getting started with STAN
#https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started

# remove.packages("rstan")
# if (file.exists(".RData")) file.remove(".RData")
# install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
#pkgbuild::has_build_tools(debug = TRUE)


# library(installr)
# updateR()

# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")

# dotR <- file.path(Sys.getenv("HOME"), ".R")
# if (!file.exists(dotR)) dir.create(dotR)
# M <- file.path(dotR, ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
# if (!file.exists(M)) file.create(M)
# cat(if( grepl("^darwin", R.version$os)) "\nCXX14FLAGS=-O3 -march=native -mtune=native -arch x86_64 -ftemplate-depth-256" else 
#   if (.Platform$OS.type == "windows") "\nCXX14FLAGS=-O3 -mtune=native -mmmx -msse -msse2 -msse3 -mssse3 -msse4.1 -msse4.2" else
#     "CXX14FLAGS = -fPIC",
#   file = M, sep = "\n", append = TRUE)


# M <- file.path(Sys.getenv("HOME"), ".R", ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
# file.edit(M) #-march=native is now known to cause problems in Windows.



# install.packages("pkgbuild")
# library(pkgbuild)
# find_rtools(TRUE)
# rtools_path()
# file.edit(file.path(Sys.getenv("HOME"), ".R", "Makevars.win")) 
# Sys.getenv("PATH")


## Fixing:
# I have reinstalled rstan, R, Rstudio, pkgbuild, rtools.
# I have removed makevars.win, and edited makevars to remove duplicates (eg CXX14...) and remove `-march=native`
# I have removed any paths from files or deleted the files that appeared in any startup, discovered using this: 
# install.packages("startup")
# startup::startup(debug = TRUE)


library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
schools_dat <- list(J = 8, 
                    y = c(28,  8, -3,  7, -1,  1, 18, 12),
                    sigma = c(15, 10, 16, 11,  9, 11, 10, 18))
#packageVersion("rstan")

fit <- stan(file = 'schools.stan', data = schools_dat)

print(fit)
plot(fit)
pairs(fit, pars = c("mu", "tau", "lp__"))

la <- extract(fit, permuted = TRUE) # return a list of arrays 
mu <- la$mu 

### return an array of three dimensions: iterations, chains, parameters 
a <- extract(fit, permuted = FALSE) 

### use S3 functions on stanfit objects
a2 <- as.array(fit)
m <- as.matrix(fit)
d <- as.data.frame(fit)





