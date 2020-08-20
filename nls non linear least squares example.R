# #web example of nls ####
# #https://stats.stackexchange.com/questions/11947/fitting-an-exponential-model-to-data

# generate data
beta <- 0.05
n <- 100
temp <- data.frame(y = exp(beta * seq(n)) + rnorm(n), x = seq(n))

# plot data
plot(temp$x, temp$y)

# fit non-linear model
mod <- nls(y ~ exp(a + b * x), data = temp, start = list(a = 0, b = 0))

# add fitted curve
lines(temp$x, predict(mod, list(x = temp$x)))



