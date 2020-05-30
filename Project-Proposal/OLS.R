library(tidyverse)
library(MASS)
library(leaps)
library(boot)

dt_location <- '/Users/nantang/Google Drive/STAT 423/Project-Proposal'
setwd(dt_location)

house_dt_new <- read.csv('house_dt_new.csv')
house_dt_new <- house_dt_new[,-1]

house_dt_new$waterfront <- as.factor(house_dt_new$waterfront)
house_dt_new$renovated <- as.factor(house_dt_new$renovated)
house_dt_new$city <- as.factor(house_dt_new$city)

# box-cox for linearity 
par(mfrow=c(1,1))
lm_fit1 <- lm(data=house_dt_new, formula = price ~.)
bc <- boxcox(lm_fit1, plotit = F)
lambda <- bc$x[which.max(bc$y)]
lambda <- 0

# backward step, BIC
lm_fit1 <- lm(data=house_dt_new, formula = price ~.)
step(object=lm_fit1, direction='backward', k=log(nrow(house_dt_new)))
## all should be included 

# forward step, BIC
lm_empty <- lm(formula=price ~ 1, data=house_dt_new)
step(object=lm_empty, direction='forward', k=log(nrow(house_dt_new)), scope=list(upper=lm_fit1, lowwer=lm_empty))
## same as backward selection

lm_fit2 <- lm(data=house_dt_new, formula=log(price) ~.)

plot(lm_fit2, which=1, pch=16, cex=0.75, col=rgb(0.2,0.2,0.2,0.3))

plot(lm_fit2, which=3)

qqnorm(lm_fit2$residuals)






