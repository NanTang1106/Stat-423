## 1-a
library(alr4)
age_dt <- sort(unique(wblake$Age))
leng_mean_dt <- numeric(length(age_dt))
for (i in age_dt) {
  leng_mean_dt[i] <- mean(wblake$Length[wblake$Age == i])
}
age_leng_mean_df <- data.frame(age_dt, leng_mean_dt)
plot(age_leng_mean_df, pch=16, ylab='Mean Length', xlab='Age', main='Age vs Mean of Length')


## 1-b
leng_sd_dt <- numeric(length(age_dt))
for (i in age_dt) {
  leng_sd_dt[i] <- sd(wblake$Length[wblake$Age == i])
}
age_leng_sd_df <- data.frame(age_dt, leng_sd_dt)
plot(age_leng_sd_df, pch=16, ylab='SD of Length', xlab='Age', main='Age vs SD of Length')


## 1-c
lm1 <- lm(wblake$Length~wblake$Scale)

plot(wblake$Scale, wblake$Length, pch=16, cex=0.5,
     xlab='Scale', ylab='Length' )
abline(lm1, col=2)

summary(lm1)


## 1-d
resid1 <- summary(lm1)$residual
hist(resid1, xlab='Residuals', main='Histogram of Residuals', xlim=c(-100, 100))
plot(lm(wblake$Length~wblake$Scale), which = 1, pch=16, cex=0.7)


## 1-e
new_pt <- data.frame(Scale=200)
fit <- lm(Length~Scale, data=wblake)
pred_200 <- predict(fit, new_pt, se.fit = T, )$fit
pred_200_CI <- predict(fit, new_pt, interval = 'confidence', level=0.95)
pred_200_PI <- predict(fit, new_pt, interval = 'prediction', level=0.95)


## 2-b
plot(x=UN1$PPgdp, y=UN1$Fertility, xlab='GDP/Person', ylab='Fertility', pch=16, cex=0.7) 

## 2-c
plot(x=log10(UN1$PPgdp), y=log10(UN1$Fertility),
     xlab='log of GDP/Person', ylab='log of Fertility', pch=16, cex=0.7) 

## 2-d

lm2 <- lm(log10(Fertility)~log10(PPgdp), data=UN1)
abline(a=lm2$coefficients[[1]], b=lm2$coefficients[[2]], col=2, lwd=2)

## 2-e
hist(lm2$residuals, xlim=c(-0.5, 0.5), xlab='Residuals', main='Histogram of Residuals')
plot(lm(log(Fertility)~log(PPgdp), data=UN1), which = 1, pch=16, cex=0.7, lwd=2)

## 2-f
beta1_est <- summary(lm2)$coefficient[2,1]
beta1_se <- summary(lm2)$coefficients[2,2]
t_val <- (beta1_est - 0) / beta1_se
n <- length(UN1$Fertility)
pt(t_val, df=n-2)

## 2-g
confidenceEllipse(lm2,grid=TRUE,xlab="Intercept",ylab="Slope",
                  main="Rice: 99% confidence region and intervals",levels=.99, col='Skyblue')
abline(h=confint(lm2,level=.99)[2,1],lty=2,lwd=2,col="firebrick")
abline(h=confint(lm2,level=.99)[2,2],lty=2,lwd=2,col="firebrick")
abline(v=confint(lm2,level=.99)[1,1],lty=2,lwd=2,col="firebrick")
abline(v=confint(lm2,level=.99)[1,2],lty=2,lwd=2,col="firebrick")
points(1.1, -0.2, col='gray25', pch=16, cex=2)
text(1.1, -0.2, pos=4, labels='(1.1, -0.2)', cex=1)

