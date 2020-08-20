#simple wilmink curve fitting for Holly

# fit wilmink lactation curves

# Load packages ####

library(ggplot2)
library(ggpmisc)
library(cowplot)
library(dplyr)
library(MASS) #used for out of date approach to nls
library(plyr) #used for out of date approach to nls
library(corrgram) #GGally has better alternatives these days?
library(car) #Scatter plot matrix

#read in data ####

data <- read.csv("2003-04StrainProds.csv")
head(data)

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

#Check data type etc
unique(dataclean$StartOfPeriod)
class(dataclean$StartOfPeriod)
mean(dataclean$StartOfPeriod)
unique(dataclean$EndOfPeriod)
class(dataclean$EndOfPeriod)
mean(dataclean$EndOfPeriod)

#Add calving date #####

CD <- aggregate(dataclean$StartOfPeriod ~ CowId, dataclean, min)
colnames(CD) <- c("CowId", "CalvingDate")

tempdata <- full_join(dataclean, CD, by = "CowId")
head(tempdata)

#Add dry off date #####

DO <- aggregate(dataclean$EndOfPeriod ~ CowId, dataclean, max)
colnames(DO) <- c("CowId", "DryoffDate")

finaldata <- full_join(tempdata, DO, by = "CowId")
head(finaldata)


#Add milk yield per day ####

finaldata$MilkYieldPerDay <- finaldata$MilkYield/finaldata$DaysInMilk

finaldata$DIM <- finaldata$StartOfPeriod + finaldata$DaysInMilk - finaldata$CalvingDate

finaldata$MaxDIM <- finaldata$DryoffDate - finaldata$CalvingDate

head(finaldata)

#my model fitting as ols ####

#plot
finaldata$CowId <- as.factor(finaldata$CowId)
p <- ggplot(data = subset(finaldata, CowId %in% c(852,1909,9855,9933,9924,9886,9926,9772,9775,1991,1982)), 
            aes(x = DIM, y = MilkYieldPerDay, fill = CowId, colour = CowId)) + 
  geom_point() +
  stat_smooth(method = 'nls', formula = y ~ a + b*exp(-.05*x) + c*x, se = FALSE, 
              method.args = list(start=c(a=31,b=-11,c=-.07), control=nls.control(maxiter=200)), colour = "green")+
  geom_smooth()
p
s <- p+ facet_wrap( ~ CowId)
s

#25.12925  -5.63838 -0.04923074

#fit wilmink and get coefficients with nls ####

#one cow
nlsobject <- nls(formula = MilkYieldPerDay  ~ a + b*exp(d*DIM) + c*DIM, 
                 data = subset(finaldata, CowId %in% c(852 )),
                 start=c(a=31,b=-11,c=-.07, d=-.05))


summary(nlsobject)
#one cow, trying to get ready for lapply
nls(formula = MilkYieldPerDay  ~ a + b*exp(-0.05*DIM) + c*DIM, 
    data = subset(finaldata, CowId %in% c(CowIds.sample[1] )),start=c(a=31,b=-11,c=-.07))


# library(MASS) #used for out of date approach to nls
# library(plyr) #used for out of date approach to nls
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

corrgram(coefs[2:4], lower.panel=panel.pie)
pairs(coefs[2:4])



#library(car)
scatterplotMatrix(~a+b+c, data=coefs)

png("coefficient plot.png")
scatterplotMatrix(~a+b+c, data=coefs)
dev.off()
