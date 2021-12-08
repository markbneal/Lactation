# suggestions to get rstan installed on RStudio Workbench/Server
## i.e. Linux machine!

# Trying this:
# https://community.rstudio.com/t/not-able-to-install-rstan/5055/16?u=mark_neal

# dotR <- file.path(Sys.getenv("HOME"), ".R")
# if (!file.exists(dotR)) dir.create(dotR)
# MAKEVARS <- file.path(dotR, "Makevars")
# if (!file.exists(MAKEVARS)) file.create(MAKEVARS)
# 
# cat(
#   "\nCXXFLAGS=-Os -mtune=native -march=native",
#   "CXXFLAGS += -Wno-unused-variable -Wno-unused-function  -Wno-unknown-pragmas",
#   "CXX=clang++",
#   file = MAKEVARS, 
#   sep = "\n", 
#   append = TRUE
# )

install.packages("rstan")

# note, with error, needed to delete non-empty directory
# R install.packages returns 'ERROR: failed to lock directory'
# https://stackoverflow.com/a/67927426/4927395
# https://linuxize.com/post/how-to-remove-files-and-directories-using-linux-command-line/

#then retry
