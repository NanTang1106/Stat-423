setwd('/Users/nantang/Google Drive/STAT 423/HW/HW3')
load("FoHF.rda")
library(corrplot)
library(carData)
library(car)
library(alr4)
library(MASS)
library(leaps)
library(boot)

## 1
## a
fit1 <- lm(formula=FoHF ~., data=FoHF)
summary(fit1)

## b
plot(fit1, which = 1, pch=16, cex=0.7)
qqnorm(fit1$residuals)

corrplot(cor(FoHF))
vif(fit1)

## d
# backward BIC
fit1 <- lm(formula=FoHF ~., data=FoHF)
step(object=fit1, direction='backward', k=log(nrow(FoHF)))

# forward BIC
fit2 <- lm(formula=FoHF ~ , data=FoHF)
step(object=fit2, direction='forward', k=log(nrow(FoHF)), scope=list(upper=fit1, lowwer=fit2))

# all variables
regsub_out <- regsubsets(FoHF~., data=FoHF)
summary(regsub_out)
which(summary(regsub_out)$bic == min(summary(regsub_out)$bic))




## 2
## a
data("BigMac2003")
fit1 <- lm(formula = BigMac~., data=BigMac2003)
vif(fit1)
corrplot(cor(BigMac2003))

## b
plot(x=BigMac2003$FoodIndex, y=BigMac2003$BigMac, 
     xlab='FoodIndex', ylab='BigMac', cex=0.7, pch=16)

## c
fit2 <- lm(formula=BigMac ~ FoodIndex, data=BigMac2003)
bc <- boxcox(fit2, plotit = T)
lambda <- bc$x[which.max(bc$y)]

plot(x=BigMac2003$FoodIndex, y=-(1/log(BigMac2003$BigMac)-1)/0.5, 
     xlab='FoodIndex', ylab='Transformed BigMac', cex=0.7, pch=16)

## d
par(mfrow=c(1,2))
plot(fit2, which=4)
plot(fit2, which=5)

## e
new_BigMac <- BigMac2003[-c(which(row.names(BigMac2003) == 'Nairobi'), which(row.names(BigMac2003) == 'Karachi')),]
fit_reduce <- lm(formula=BigMac ~ FoodIndex, data=new_BigMac)
bc_reduce <- boxcox(fit_reduce, plotit = T)
lambda_reduce <- bc_reduce$x[which.max(bc_reduce$y)]

plot(x=new_BigMac$FoodIndex, y=log(new_BigMac$BigMac), 
     xlab='FoodIndex', ylab='Transformed BigMac', cex=0.7, pch=16)

## f
par(mfrow=c(3,3))
names <- colnames(BigMac2003)
for (i in 2:9) {
  hist(BigMac2003[,i], main=paste('Histogram of',names[i]), xlab=names[i])
}
par(mfrow=c(1,1))

## g
loocv.lm <- function(mdl) {
  return(mean((residuals(mdl)/(1-hatvalues(mdl)))^2)) 
  }

model1 <- lm(formula=log(BigMac)~log(Bread) + log(Rice), data=new_BigMac)
model2 <- lm(formula=log(BigMac)~log(Bread) + log(Rice) + Apt + log(Bus), data=new_BigMac)
model3 <- lm(formula=log(BigMac)~log(Bread) + log(Rice) + Apt + log(Bus) + log(TeachNI), data=new_BigMac)

c(loocv.lm(model1), loocv.lm(model2), loocv.lm(model3))

## h
par(mfrow=c(1,2))
plot(model3, which = 1, pch=16, cex=0.7)
qqnorm(model3$residuals)

plot(model3, which=4)
plot(model3, which=5)

par(mfrow=c(1,1))






