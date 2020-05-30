library(tidyverse)
library(glmnet)
library(sfsmisc)

dir <- '/Users/nantang/Google Drive/STAT 423/HW/HW4'
setwd(dir)

## 1
## a
car_dt <- readRDS('car.rds')
fit_car1 <- glm(data=car_dt, formula=purchase~income+age, family = binomial)
summary(fit_car1)

##b
coeff_income <- fit_car1$coefficients[[2]]
coeff_age <- fit_car1$coefficients[[3]]
exp(coeff_income)
exp(coeff_age)

##c 
prob1 <- predict(fit_car1, data.frame(income=50, age=3), type='response')

##d
par(mfrow=c(1,2))
plot(fit_car1, which = 4)
plot(fit_car1, which=5)
par(mfrow=c(1,1))

##f
fit_car2 <- glm(data=car_dt, formula=purchase~income+age+income*age)
anova(fit_car1, fit_car2, test='Chisq')


##2
no.yes <- c("No", "Yes")
smoking <- gl(2,1,7, no.yes)
obesity <- gl(2,2,7, no.yes)
snoring <- gl(2,4,7, no.yes)
n.total <- c(60, 17, 8, 187, 85, 51, 23)
n.hyper <- c(5, 2, 1, 35, 13, 15, 8)

##a
resp_matrix <- data.frame(n.hyper, (n.total-n.hyper))
colnames(resp_matrix) <- c('with_hypertension', 'without_hypertension')

##b
fit_hyper1 <- glm(n.hyper/n.total~smoking+obesity+snoring, weight=n.total, family=binomial)
hyper_df <- fit_hyper1$df.residual
pchisq(fit_hyper1$null.deviance - fit_hyper1$deviance, hyper_df, lower.tail = F)

##c
drop1(fit_hyper1, test='Chisq')

##d
fit_hyper2 <- glm(n.hyper/n.total~obesity+snoring, weight=n.total, family=binomial)
drop1(fit_hyper2, test='Chisq')

##e
hyper_dt <- data.frame(smoking, obesity, snoring, n.hyper, (n.total-n.hyper), (n.hyper/n.total))
colnames(hyper_dt) <- c('smoking', 'obesity', 'snoring', 'hyper', 'non_hyper', 'obs_prop')

hyper_dt <- hyper_dt %>%
  mutate(pred_prop = predict(fit_hyper2, hyper_dt[,1:3], type='response')) %>%
  mutate(pred_count = round(pred_prop * n.total)) %>%
  select(smoking, obesity, snoring, non_hyper, hyper, pred_count, obs_prop, pred_prop)

##3
##a
set.seed(1)
data(ozone, package='gss')
ozone_dt <- ozone
ozone_dt$logupo3 <- log(ozone_dt$upo3)
ozone_dt <- select(ozone_dt, -upo3)
ozone_dt <- ozone_dt[-92,]

ff <- wrapFormula(logupo3~., data=ozone_dt, wrapString = 'poly(*, degree=3)')
ff <- update(ff, logupo3~.^3)
mm <- model.matrix(ff, data=ozone_dt)

fit_ridge <- glmnet(mm, y=scale(ozone_dt$logupo3, scale=F), alpha=0, intercept=F)
fit_lasso <- glmnet(mm, y=scale(ozone_dt$logupo3, scale=F), alpha=1, intercept=F)
fit_elnet <- glmnet(mm, y=scale(ozone_dt$logupo3, scale=F), alpha=0.5, intercept=F)

plot(fit_ridge, xvar='lambda', main='Ridge Regression')
plot(fit_lasso, xvar='lambda', main='Lasso Regression')
plot(fit_elnet, xvar='lambda',main='Elastic Net Regression')

##b
set.seed(1)
cv.eln <- cv.glmnet(mm, y=scale(ozone_dt$logupo3, scale=F), alpha=0.5, nfolds=10, intercept=F)
plot(cv.eln)

cv.eln$lambda.1se


##4
##a
load('CustomerWinBack.rda')
glimpse(cwb)
cwb$gender <- as.factor(cwb$gender)

fitlm_cwb <- lm(data=cwb, formula=duration~.)
summary(fitlm_cwb)

par(mfrow=c(2,2))
plot(fitlm_cwb, which=1)
plot(fitlm_cwb, which=2)
plot(fitlm_cwb, which=4)
plot(fitlm_cwb, which=5)

par(mfrow=c(1,1))


##b
step(object=fitlm_cwb, direction='backward')

# forward BIC
fit_empty <- lm(data=cwb, formula=duration~1)
step(object=fit_empty, direction='forward', scope=list(upper=fitlm_cwb, lowwer=fit_empty))


##c
step(object=fitlm_cwb, direction='backward', k=log(nrow(cwb)))

step(object=fit_empty, direction='forward', k=log(nrow(cwb)),
     scope=list(upper=fitlm_cwb, lowwer=fit_empty))

##d
xx <- model.matrix(duration~0+., cwb)[,-4] 
yy <- cwb$duration

set.seed(1)
cv_ridge <- cv.glmnet(xx, yy, nfolds=5, alpha=0)
opt_lambda1 <- cv_ridge$lambda.1se
fitrg_cwb <- glmnet(xx, yy, alpha=0, lambda = opt_lambda1)
coef(fitrg_cwb)

##e
set.seed(1)
cv_lasso <- cv.glmnet(xx, yy, nfolds = 5, alpha=1)
opt_lambda2 <- cv_lasso$lambda.1se
fitlas_cwb <- glmnet(xx, yy, alpha=1, lambda =opt_lambda2)
coef(fitlas_cwb)

##f
pre.ols <- c() 
pre.aic <- c() 
pre.bic <- c() 
pre.rr <- c() 
pre.las <- c() 
folds <- 5
sb <- round(seq(0,nrow(cwb),length=(folds+1)))


for (i in 1:folds){
  ## define training and test datasets
  test <- (sb[((folds+1)-i)]+1):(sb[((folds+2)-i)]) 
  train <- (1:nrow(cwb))[-test]
  ## fit models
  fit.ols <- lm(duration ~ ., data=cwb[train,]) 
  fit.aic <- lm(duration ~ offer + lapse + gender + price, data=cwb[train,]) 
  fit.bic <- lm(duration ~ offer + lapse + gender + price, data=cwb[train,])
  xx <- model.matrix(duration~0+., cwb[train,])[,-4] 
  yy <- cwb$duration[train]
  fit.rr <- glmnet(xx,yy, lambda = opt_lambda1, alpha = 0) 
  
  fit.las <- glmnet(xx,yy, lambda = opt_lambda2, alpha = 1)
  ## create predictions
  pre.ols[test] <- predict(fit.ols, newdata=cwb[test,])
  pre.aic[test] <- predict(fit.aic, newdata=cwb[test,])
  pre.bic[test] <- predict(fit.bic, newdata=cwb[test,])
  pre.rr[test] <- model.matrix(duration~., cwb[test,])%*%as.numeric(coef(fit.rr)) 
  pre.las[test] <- model.matrix(duration~., cwb[test,])%*%as.numeric(coef(fit.las))
}
## Finally, compute the mean squared prediction error:
mean((cwb$duration-pre.ols)^2) 
mean((cwb$duration-pre.aic)^2) 
mean((cwb$duration-pre.bic)^2) 
mean((cwb$duration-pre.rr)^2) 
mean((cwb$duration-pre.las)^2)

