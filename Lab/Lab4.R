# Homework 2 and Homework 3 from year 2019
# 
# setwd("~/Google Drive/Stat 423 : Stat 504 Wi2020/Homework/HW 2019/hw2/")
rm(list = ls())

# Problem 1.
# load data
basic = readRDS("basic.RDS")

# part (a)
summary(basic)

pairs(~ph + l.sar + height + h.quad, data = basic, main = "PH and 
tree growth", subset = height <= 3)
pairs(basic[c(1:2, 4)], main = "PH and tree growth", upper.panel = NULL)

pairs(basic, main = "PH and tree growth", pch = 21)
pairs(basic, main = "PH and tree growth", pch = 21, upper.panel = NULL)

# remove missing values in the data
basic[which(basic$height == NA)]  # don't do this
basic[which(is.na(basic$height))]  # slightly better, although doesn't work
basic[which(is.na(basic$height)), ]

basic[which(is.na(basic$height) | is.na(basic$h.quad)), ] 
with(basic, basic[which(is.na(height) | is.na(h.quad)), ])

# even better
library(dplyr)
basic %>% filter(is.na(height) | is.na(h.quad))

basic_complete = basic[complete.cases(basic), ]
nrow(basic_complete)

# part (b)
summary(lm(h.quad ~ ph + l.sar, data = basic_complete))


# Problem 2
# part (a)
library(alr4)
water.data <- water[,c("BSAAM","OPBPC","OPRC","OPSLAKE")]
pairs(water.data)

cor(water.data)

# part (b)
summary(lm(formula = BSAAM ~ ., data = water.data))

# part (c)
# i
p.vals <- summary(lm(formula = BSAAM ~ ., 
                     data = water.data))$coefficients[2:4,4]
p.vals < 0.01

# ii
p.bonf <- p.adjust(p.vals, method="bonferroni")
p.bonf < 0.01

p.vals < 0.01/3

# iii
#  Holm
sort(p.vals)
holm_p_val <- 0.01 / c(3, 2, 1)

sort(p.vals) < holm_p_val

# iv
# Benjamini-Hochberg procedure
sort(p.vals)
fdr <- 0.01/3*c(1,2,3)

sort(p.vals) < fdr

p.adjust.fdr <- p.adjust(p.vals, method = "fdr")

## Homework 3





