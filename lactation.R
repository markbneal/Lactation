# fit wilmink lactation curves, prepare data for STAN Bayesian fitting model

#rm(list=ls(all=TRUE))

#read in data ####
data <- read.csv("2003-04StrainProds.csv")
head(data)
library(ggplot2)
library(ggpmisc)
library(cowplot)
library(dplyr)

#plot all cows
p <- ggplot(data = data, aes(x = PeriodNo, y = MilkYield, fill = CowId)) + 
  geom_point() 
p
unique(data$CowId)
CowIds <- as.vector(unique(data$CowId))
CowIds.list <- split(CowIds, seq(length(CowIds)))

class(CowIds.list[1])
CowIds.list[1]

# CowIds.sample <- c(852,1909,9855,9933,9924,9886,9926,9772,9775,1991,1982)
CowIds.sample <- sort(c(852,1909,9855,9933,9924,9886,9926,1991,1982) )#removed low dim/early dryoff
class(CowIds.sample)
length(CowIds.sample)

unique(data$PeriodFinish)

#plot some cows and facet (milk per week period) ####
p <- ggplot(data = subset(data, CowId %in% c(852,1909,9855,9933,9924,9886,9926,9772,9775,1991,1982)), aes(x = PeriodNo, y = MilkYield, fill = CowId)) + 
  geom_point() +
  geom_smooth()
p
s <- p+ facet_wrap( ~ CowId)
s

#add days in milk for lactation ####
class(data$PeriodFinish)
data$Date <- as.Date(data$PeriodFinish, format = "%d/%m/%Y")
head(data)

dataclean <- data[!is.na(data$MilkYield), ]
head(dataclean)
#week that milk first supplied
#mindate <- min(dataclean$Date)
dataclean$StartOfPeriod <- (as.numeric(dataclean$Date) - dataclean$DaysInMilk)
#week that milk last supplied
#maxdate <- max(dataclean$Date)
dataclean$EndOfPeriod <- (as.numeric(dataclean$Date) - 7 + dataclean$DaysInMilk)
head(dataclean)

unique(dataclean$StartOfPeriod)
class(dataclean$StartOfPeriod)
mean(dataclean$StartOfPeriod)
unique(dataclean$EndOfPeriod)
class(dataclean$EndOfPeriod)
mean(dataclean$EndOfPeriod)

#Add calving date #####

CD <- aggregate(dataclean$StartOfPeriod ~ CowId, dataclean, min)
colnames(CD) <- c("CowId", "CalvingDate")

#?left_join
tempdata <- full_join(dataclean, CD, by = "CowId")
head(tempdata)

#Add dry off date #####

DO <- aggregate(dataclean$EndOfPeriod ~ CowId, dataclean, max)
colnames(DO) <- c("CowId", "DryoffDate")

#?left_join
finaldata <- full_join(tempdata, DO, by = "CowId")
head(finaldata)


#Add milk yield per day ####

finaldata$MilkYieldPerDay <- finaldata$MilkYield/finaldata$DaysInMilk

finaldata$DIM <- finaldata$StartOfPeriod + finaldata$DaysInMilk - finaldata$CalvingDate

finaldata$MaxDIM <- finaldata$DryoffDate - finaldata$CalvingDate

head(finaldata)

#my model fitting as ols ####
# finaldata$CowId <- as.factor(finaldata$CowId)
# # pre-process exponential component ####
# finaldata$expPart <- exp(-0.05*finaldata$DIM)
# 
# mod <- lm(MilkYieldPerDay ~ 0 + 1 + I(exp(-0.05*DIM)) + DIM, data = subset(finaldata, CowId %in% c(854)))
# #mod <- nls(MilkYieldPerDay ~ expPart + DIM, data = finaldata, start = list(expPart = -11, DIM = -0.07))  not working
# summary(mod)
# 
# mod <- lm(MilkYieldPerDay ~ 0 + CowId + CowId*I(exp(-0.05*DIM)) + CowId*DIM, data = subset(finaldata, CowId %in% c(852,1909,9855,9933,9924,9886,9926,9772,9775,1991,1982)))
# #mod <- nls(MilkYieldPerDay ~ expPart + DIM, data = finaldata, start = list(expPart = -11, DIM = -0.07))  not working
# summary(mod)
# #plot(finaldata$DIM, finaldata$MilkYieldPerDay)
# plot(subset(finaldata, CowId %in% c(854))$DIM, subset(finaldata, CowId %in% c(854))$MilkYieldPerDay)
# points(finaldata$DIM, predict(mod, list(DIM = finaldata$DIM)), col="red")

#plot with daily milk yield  and fitted wilmink curves ####
my.linear.formula <- y ~ x
my.poly2.formula <- y ~ x + I(x^2)

#fitted bayesian cofficients - but they vary by facet (Cow)
# fun.1 <- function(x) 25.12925 -5.63838*exp(-.05*x) -0.04923074*x
# 
# f <- 2
# fun.1 <- function(x) allcoefs[f,2] +allcoefs[f,3]*exp(-.05*x) +allcoefs[f,4]*x
# 
# CowIds

#plot
finaldata$CowId <- as.factor(finaldata$CowId)
p <- ggplot(data = subset(finaldata, CowId %in% c(852,1909,9855,9933,9924,9886,9926,9772,9775,1991,1982)), 
            aes(x = DIM, y = MilkYieldPerDay, fill = CowId, colour = CowId)) + 
  geom_point() +
  stat_smooth(method = 'nls', formula = y ~ a + b*exp(-.05*x) + c*x, se = FALSE, 
              method.args = list(start=c(a=31,b=-11,c=-.07), control=nls.control(maxiter=200)), colour = "green")+
  #stat_function(fun = fun.1)+
  geom_smooth()
p
s <- p+ facet_wrap( ~ CowId)
s

#??stat_poly_eq
#help("stat_smooth")
#25.12925  -5.63838 -0.04923074

#fit wilmink and get coefficients with nls ####
#?nls
#one cow
nlsobject <- nls(formula = MilkYieldPerDay  ~ a + b*exp(d*DIM) + c*DIM, data = subset(finaldata, CowId %in% c(852 )),start=c(a=31,b=-11,c=-.07, d=-.05))
summary(nlsobject)
#one cow, trying to get ready for lapply
                          nls(formula = MilkYieldPerDay  ~ a + b*exp(-0.05*DIM) + c*DIM, data = subset(finaldata, CowId %in% c(CowIds.sample[1] )),start=c(a=31,b=-11,c=-.07))

#lapply not working, Cowid list is not appropriately formatted?
                          # CowFits  <- lapply(CowIds.sample, function(dvar)nls(formula = y ~ a + b*exp(-0.05*x) + c*x,     
#                                         data = subset(finaldata, CowId %in% c(dvar)),
#                                         start=c(a=31,b=-11,c=-.07) ) )
# ?lapply

require(MASS); require(plyr)
# SPLIT-APPLY-COMBINE
# from https://stackoverflow.com/questions/9014308/r-extract-regression-coefficients-from-multiply-regression-via-lapply-command
regressions <- dlply(finaldata, .(CowId), nls, formula = MilkYieldPerDay  ~ a + b*exp(-0.05*DIM) + c*DIM, start=c(a=31,b=-11,c=-.07), control=nls.control(maxiter=2000) )
coefs <- ldply(regressions, coef)
class(coefs)
coefsnls <- subset(coefs, CowId %in% c(CowIds.sample))
coefsnls$method <- "nls"


#look at coefficient distributions ####
p <- ggplot()+
    geom_density(data = coefs, aes(x = a ))
p
p <- ggplot()+
  geom_density(data = coefs, aes(x = b ))
p
p <- ggplot()+
  geom_density(data = coefs, aes(x = c ))
p

#install.packages("corrgram")
library(corrgram)

corrgram(coefs[2:4], lower.panel=panel.pie)
pairs(coefs[2:4])
library(car)
scatterplotMatrix(~a+b+c, data=coefs)

png("coefficient plot.png")
scatterplotMatrix(~a+b+c, data=coefs)
dev.off()

#scatterplotMatrix(~a+b+c|lactation, data=coefs)

#put coefficients into custom equations for plotting on ggplot facets?


#to do ####
#grid up likely values of a, b and c coefficients - DONE
#restrictions on coefficient values? - Not necessary
#Bayesian priors over regression coefficients? - DONE
#incorporate restrictions on total MS - query??
#restriction on MS per months (or Bayesian?) -  query??
#guess dry off date? - query??


#make data frame for stan
##cow rows by DIMday columns and save to space delimited

shortcowdata <- subset(finaldata, CowId %in% CowIds.sample)
colnames(shortcowdata)
shortcowdata2 <- subset(shortcowdata, select=c("CowId", "DIM", "MilkYieldPerDay"))
ddply(shortcowdata2, .(CowId), summarise, Value = max(DIM))
shortcowdata2$days <- round(shortcowdata2$DIM/7,0)*7
ddply(shortcowdata2, .(CowId), summarise, Value = max(days))

#subset to "balanced" dataset
shortcowdata3 <- subset(shortcowdata2, days <= 245)
shortcowdata3 <- subset(shortcowdata3, days > 0)
dim(shortcowdata3)

#delete unnecessary columns
shortcowdata4 <- subset(shortcowdata3, select=c("CowId", "days", "MilkYieldPerDay"))
# spread to wide
library(tidyr)
widecows <- spread(shortcowdata4, key = days, value = MilkYieldPerDay)
widecows[,1] <- as.numeric(levels(widecows[,1]))[widecows[,1]]
widecows

#sort by cowid
widecows <- widecows[order(widecows$CowId),]
cowkey <- widecows[,1]
class(cowkey)

#rename columns?
colnames(widecows) <- paste("day", colnames(widecows), sep = "")
colnames(widecows)[1] <- "CowId"
widecows
#round columns
widecows <- widecows %>% mutate_if(is.numeric, round,1)

#delete cow id column
widecows <-  widecows[,-1]
# save
write.table(widecows, "cows.txt", row.names = FALSE, sep=" ",quote = FALSE)

#herd data
herdtotal <- read.csv("herd_total.csv")
herdtotalshort <- herdtotal[-1]
write.table(herdtotalshort, "herd_total.txt", row.names = FALSE, sep=" ",quote = FALSE)

herdtocow <- read.csv("herd_to_cow.csv")
herdtocowshort <- herdtocow[-1]
write.table(herdtocowshort, "herd_to_cow.txt", row.names = FALSE, sep=" ",quote = FALSE)

#cow data
cowtotal <- read.csv("cow_total.csv")
cowtotalshort <- cowtotal[-1]
write.table(cowtotalshort, "cow_total.txt", row.names = FALSE, sep=" ",quote = FALSE)



#Example 2, rats, which is now converted to cows ####
#Data: 9 cows with milk yield observations per week for 35 weeks
#Model: Fitting Wilmink curves (Wilmink (1987) Livestock Production Science 16:321-334)
#Priors: Very loosely based on observations for 500 cows fitted by nls, assumed normal, but standard deviation is inflated.

#install.packages("rstan")
#install.packages("processx")
library("rstan")
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)
#Sys.setenv(LOCAL_CPPFLAGS = '-march=native') #not for windows

y <- as.matrix(read.table('cows.txt', header = TRUE))
x <- seq(7,by=7,length.out=35)
z <- as.vector(read.table('herd_total.txt', header = TRUE)) 
z <- as.numeric(z[,1])
w <- as.matrix(read.table('herd_to_cow.txt', header = TRUE)) 
v <- as.vector(read.table('cow_total.txt', header = TRUE)) 
v <- as.numeric(v[,1])

class(x)
class(z)
N <- as.numeric(nrow(y))
T <- ncol(y)
H <- length(z)
#rstan_options(auto_write = TRUE)
cows_fit <- stan(file = 'cows.stan', data=c("N","T","H","x","y","z","w","v")) #takes about 20 seconds with no errors for individual fit, 60s with no rds file

print(cows_fit)
summary(cows_fit)
cows_fit@stanmodel@dso <- new("cxxdso") 
saveRDS(cows_fit, file = "fitcurrent.rds")
#saveRDS(cows_fit, file = "fitwithonlydailydata.rds")
#saveRDS(cows_fit, file = "fitwithonlycumulative.rds")
cows_fit_current <- readRDS("fitcurrent.rds")
cows_fit_daily <- readRDS("fitwithonlydailydata.rds")
cows_fit_annual <- readRDS("fitwithonlycumulative.rds")
resultscurrent <- as.data.frame(cows_fit_current@.MISC$summary$c_msd)[1]
resultsd <- as.data.frame(cows_fit_daily@.MISC$summary$c_msd)[1]
resultsa <- as.data.frame(cows_fit_annual@.MISC$summary$c_msd)[1]

#cows_fit <- readRDS("fitwithonlycumulative.rds")


library(tibble)
library(dplyr)
compare <- full_join(rownames_to_column(resultsd), rownames_to_column(resultscurrent), by = ("rowname" = "rowname"))
#compare <- cbind(resultsd,resultsa)
#install.packages("xlsx")
#install.packages("rJava")
library(xlsx)
write.xlsx(compare, "compare.xlsx")

?plot
plot(cows_fit)
plot(cows_fit, show_density = TRUE, ci_level = 0.5, fill_color = "purple")
plot(cows_fit, plotfun = "hist", pars = "alpha", include = FALSE)
plot(cows_fit, plotfun = "trace", pars = c("alpha", "beta","chi"), inc_warmup = TRUE)
plot(cows_fit, plotfun = "rhat")

plot(cows_fit, pars=c("alpha[1]"))
#pairs(cows_fit, pars = c("mu_alpha", "mu_beta", "mu_chi", "mu_kappa", "lp__"))
pairs(cows_fit, pars = c("mu_alpha", "mu_beta", "mu_chi", "lp__"))
#pairs(cows_fit, pars = c("alpha[4]", "beta[4]", "chi[4]","kappa[4]"))
pairs(cows_fit, pars = c("alpha[4]", "beta[4]", "chi[4]"))
results <- cows_fit@.MISC$summary$c_msd
resultsdf <- as.data.frame(results)
pars_estimated <- rownames(resultsdf)
pars_estimated
length(pars_estimated)
plot(cows_fit, pars=pars_estimated[1:9])
plot(cows_fit, pars=pars_estimated[10:18])
plot(cows_fit, pars=pars_estimated[19:27])
plot(cows_fit, pars=pars_estimated[28:35])


# plot results like from http://mc-stan.org/bayesplot/
#install.packages("bayesplot")
library(bayesplot)
#install.packages("rstanarm")
library(rstanarm)

color_scheme_set("purple")

fit <- stan_glmer(mpg ~ wt + (1|cyl), data = mtcars)
ppc_intervals(
  y = mtcars$mpg,
  yrep = posterior_predict(fit),
  x = mtcars$wt,
  prob = 0.5
) +
  labs(
    x = "Weight (1000 lbs)",
    y = "MPG",
    title = "50% posterior predictive intervals \nvs observed miles per gallon",
    subtitle = "by vehicle weight"
  ) +
  panel_bg(fill = "gray95", color = NA) +
  grid_lines(color = "white")


#//  peak_t_est = -20 * log(mu_chi * 20 / mu_beta) * 7;  //not always defined, due to current priors not restricted to positive values?
#//  peak_y_est = mu_alpha + mu_beta * exp(-0.05 * peak_t_est) + mu_chi * peak_t_est; //not always defined, when peak_t_est isn't defined

chain <- resultsdf[1]
#plot correl
coefsbayes <- as.data.frame(cowkey)
coefsbayes$a <- chain[1:9,1]
coefsbayes$b <- chain[10:18,1]
coefsbayes$c <- chain[19:27,1]
#coefsbayes$id <- 1:9
coefsbayes$method <- "bayes"
colnames(coefsbayes)[1] <- "CowId"

pairs(subset(coefsbayes, select=c("a", "b", "c")))
corrgram(subset(coefsbayes, select=c("a", "b", "c")), lower.panel = panel.pie)


#compare nls and bayes results ####

allcoefs <- rbind(coefsbayes,coefsnls)

ggplot(data=allcoefs, aes(x=a, colour = method))+
  geom_density()
ggplot(data=allcoefs, aes(x=b, colour = method))+
  geom_density()
ggplot(data=allcoefs, aes(x=c, colour = method))+
  geom_density()

allcoefs <- cbind(coefsbayes,coefsnls)

for (i in 1:length(allcoefs)) {
print(ggplot(data.frame(x=c(0,365)), aes(x)) +
  stat_function(fun=function(x)allcoefs[i,2]+allcoefs[i,3]*exp(-0.05*x)+allcoefs[i,4]*x, geom="line", aes(colour="bayes")) +
  stat_function(fun=function(x)allcoefs[i,7]+allcoefs[i,8]*exp(-0.05*x)+allcoefs[i,9]*x, geom="line", aes(colour="nls")) )
}

i <- c(1)
#plotting nls vs bayes for each cow.
# lapply(i,
# print(ggplot(data.frame(x=c(0,365)), aes(x)) +
#         stat_function(fun=function(x)allcoefs[i,2]+allcoefs[i,3]*exp(-0.05*x)+allcoefs[i,4]*x, geom="line", aes(colour="bayes")) +
#         stat_function(fun=function(x)allcoefs[i,7]+allcoefs[i,8]*exp(-0.05*x)+allcoefs[i,9]*x, geom="line", aes(colour="nls")) )
# )


for(i in 1:9) {
       print(ggplot(data.frame(x=c(0,365)), aes(x)) +
               stat_function(fun=function(x)allcoefs[i,2]+allcoefs[i,3]*exp(-0.05*x)+allcoefs[i,4]*x, geom="line", aes(colour="bayes")) +
               stat_function(fun=function(x)allcoefs[i,7]+allcoefs[i,8]*exp(-0.05*x)+allcoefs[i,9]*x, geom="line", aes(colour="nls")) )
}

p <- ggplot(data.frame(x=c(0:365)), aes(x)) +
        stat_function(fun=function(x)mean(allcoefs[,2])+mean(allcoefs[,3])*exp(-0.05*x)+mean(allcoefs[,4])*x, geom="line", aes(colour="bayes")) +
        stat_function(fun=function(x)mean(allcoefs[,7])+mean(allcoefs[,8])*exp(-0.05*x)+mean(allcoefs[,9])*x, geom="line", aes(colour="nls")) 
p
# s <- p+
#   facet_wrap( ~ allcoefs$CowId)
# s

#allcoefs[9,2]

class(y)
ydf <- as.data.frame(y)
ydf
ydf$CowID <- cowkey #check


#plot data as STAN see's it ####

library(tidyr)
ydf_long <- gather(ydf, DIMday, MilkYieldPerDay, day7:day245, factor_key=TRUE)
ydf_long
ydf_long_clean <- as.data.frame(sapply(ydf_long,gsub,pattern="week",replacement=""))
ydf_long_clean$cow <- as.factor(as.character(ydf_long_clean$CowID))
ydf_long_clean$DIMday <- as.numeric(gsub("day","",as.character(ydf_long_clean$DIMday)))
ydf_long_clean$MilkYieldPerDay <- as.numeric(as.character(ydf_long_clean$MilkYieldPerDay))
plot(ydf_long_clean$DIMday,ydf_long_clean$MilkYieldPerDay)

p <- ggplot(data = ydf_long_clean, aes( x=DIMday, y = MilkYieldPerDay, colour = cow))+
  geom_point()
p
s <- p+ facet_wrap( ~ ydf_long_clean$cow) #do subset if too many cows!
s



#fitted bayesian cofficients - but they vary by facet (Cow)
#fun.1 <- function(x) 25.12925 -5.63838*exp(-.05*x) -0.04923074*x
finaldata$CowId <- as.factor(finaldata$CowId)

# f <- 2
# fun.1 <- function(x) allcoefs[f,2] +allcoefs[f,3]*exp(-.05*x) +allcoefs[f,4]*x

#plot function
my_plot_function <- function(f) {
  fun.1 <- function(x) allcoefs[f,2] +allcoefs[f,3]*exp(-.05*x) +allcoefs[f,4]*x
  p <- ggplot(data = subset(finaldata, CowId %in% CowIds.sample[f]), 
              aes(x = DIM, y = MilkYieldPerDay, fill = CowId, colour = CowId)) + 
    geom_point() +
    stat_smooth(method = 'nls', formula = y ~ a + b*exp(-.05*x) + c*x, se = FALSE, 
              method.args = list(start=c(a=31,b=-11,c=-.07), control=nls.control(maxiter=200)), colour = "green")+
    stat_function(fun = fun.1, colour="blue")+
    #geom_smooth()
    geom_smooth (alpha=0.1, linetype=0, colour="blue")+
    stat_smooth (geom="line", alpha=0.3, size=1)+
    annotate(geom="text", x=100,y=15,label="Locally weighted regression", colour = "red")+
    annotate(geom="text", x=100,y=13,label="Non linear fit", colour = "green")+
    annotate(geom="text", x=100,y=11,label="Bayesian non linear fit", colour = "blue")
  return(p)
}

my_plot_function(1)
#make list of plots
plist <- list()
for (i in 1:length(CowIds.sample)) {
  plist[[i]] <- my_plot_function(i)
  plot(plist[[i]])
}
#plist

plotbox <- plot_grid(plist[[1]],plist[[2]],plist[[3]],
                     plist[[4]],plist[[5]],plist[[6]],
                     plist[[7]],plist[[8]],plist[[9]], nrow=3, align="hv")
plotbox

save_plot("Wilmink lactation curves - naive and bayesian.png", plotbox, base_height=8, base_width=14)



