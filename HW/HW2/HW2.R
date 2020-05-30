library(alr4)
library(dplyr)
library(ggplot2)

## question 1
## a
bgs_girls <- BGSgirls %>% select(WT2, HT2, WT9, HT9, ST9, BMI18)

pairs(bgs_girls, cex=0.6, pch=16)
print(cor(bgs_girls))

## b
fit1 <- lm(formula=BMI18~WT9 + ST9, data=bgs_girls)
fit2 <- lm(formula=BMI18 ~., data=bgs_girls)

## c
n <- nrow(bgs_girls)
sse_full <- sum(residuals(fit2)^2)
sse_red <- sum(residuals(fit1)^2)
df_full <- n - 6
df_red <- n - 3
f_value <- ((sse_red - sse_full) / (df_red - df_full)) / (sse_full / df_full)

anova(fit1, fit2)

## d
resid_full <- residuals(fit2)
hist(resid_full, xlim=c(-10, 10), xlab='Residuals', 
     main='Histogram of Residuals', probability = T)

plot(fit2, which=1, pch=16, cex=0.75)

## f
p_val <- summary(fit2)$coefficients[2:6, 4]
p_val < 0.1/5

holm_val <- 0.1 / (5:1)
sort(p_val) < holm_val

fdr_val <- 0.1 * (1:5) / 5
sort(p_val) < fdr_val


## question 2
# a
bgs_all <- BGSall %>% select(HT18, HT9, HT2, Sex) %>% 
  mutate(Sex = replace(Sex, Sex==0, 'Men')) %>%
  mutate(Sex = replace(Sex, Sex=='1', 'Women'))

bgs_all$Sex <- as.factor(bgs_all$Sex)

p <- ggplot(data=bgs_all, mapping=aes(y=HT18, x=HT9)) + 
  geom_point(aes(col=Sex)) + 
  theme_bw()

# b
fit1 <- lm(data=bgs_all, formula = HT18 ~ HT9 + Sex)
women_inter <- summary(fit1)$coefficients[3, 1] + summary(fit1)$coefficients[1, 1]

new_pt <- data.frame(Sex='Women', HT9=135)
predict(fit1, new_pt)

new_pt2 <- data.frame(Sex='Women', HT9=137)
predict(fit1, new_pt2)

se_inter <- summary(fit1)$coefficients[3,2]
diff_inter <- summary(fit1)$coefficients[3,1]
c(diff_inter + se_inter * qt(0.95, df=133), diff_inter - se_inter * qt(0.95, df=133))

# c
fit_height <- lm(data=bgs_all, formula = HT18 ~ HT9 + Sex)
fit_height2 <- lm(data=bgs_all, formula = HT18 ~ HT2 + HT9 + Sex )
fit_height3 <- lm(data=bgs_all, formula = HT18 ~ HT2 + HT9 + Sex + Sex:HT2 + Sex:HT9)
fit_height4 <- lm(data=bgs_all, formula = HT18 ~ HT2 + HT9 + HT2:HT9 + Sex + Sex:HT2 + Sex:HT9 + Sex:HT2:HT9)

pred_pt1 <- data.frame(HT2=90, HT9=135, Sex='Women')
predict(fit_height2, pred_pt1)
predict(fit_height3, pred_pt1)
predict(fit_height4, pred_pt1)

anova(fit_height, fit_height2)
anova(fit_height2, fit_height3)

anova(fit_height3, fit_height4)
anova(fit_height2, fit_height3)

n <- nrow(bgs_all)
sse_1 <- sum(residuals(fit_height)^2)
sse_2 <- sum(residuals(fit_height2)^2)
sse_3 <- sum(residuals(fit_height3)^2)
sse_4 <- sum(residuals(fit_height4)^2)
df_all <- c(n-3, n-4, n-6, n-8)
sse_all <- c(sse_1, sse_2, sse_3, sse_4)
sse_all / df_all
