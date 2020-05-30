rm(list=ls(all.names=T))

library(grid)
library(tile)
library(simcf)
library(MASS)


dt_location <- '/Users/nantang/Google Drive/STAT 423/Project-Proposal'
setwd(dt_location)

house_dt <- read.csv('house_dt_new.csv')
house_dt <- house_dt[,-1]
house_dt$waterfront <- as.character(house_dt$waterfront)
house_dt$renovated <- as.character(house_dt$renovated)

model1 <- log(price)~floors + waterfront + view + condition + sqft_basement + lat + long + sqft_lot15 +
  room_avg_sqft + house_age + renovated

lm1 <- lm(data=house_dt, model1)
summary(lm1)

model2 <- log(price)~floors + condition + sqft_basement + lat + long + sqft_lot15 +
  room_avg_sqft + house_age

lm2 <- lm(data=house_dt, model2)
lm2_pe <- lm2$coefficients
lm2_vc <- vcov(lm2)

## set conditional counterfactuals for continuous variables
xscen <- cfMake(model2, data=house_dt, nscen=8)

xscen <- cfName(xscen, 'Floors + 0.5 sd', scen=1)
xscen <- cfChange(xscen, 'floors', 
                  x=mean(house_dt$floors) + 0.5 * sd(house_dt$floors),
                  scen=1)

xscen <- cfName(xscen, 'Condition + 0.5 sd', scen=2)
xscen <- cfChange(xscen, 'condition', 
                  x=mean(house_dt$condition) + 0.5 * sd(house_dt$condition),
                  scen=2)

xscen <- cfName(xscen, 'Sqft of Basement + 0.5 sd', scen=3)
xscen <- cfChange(xscen, 'sqft_basement', 
                  x=mean(house_dt$sqft_basement) + 0.5 * sd(house_dt$sqft_basement),
                  scen=3)

xscen <- cfName(xscen, 'Latitude + 0.5 sd', scen=4)
xscen <- cfChange(xscen, 'lat', 
                  x=mean(house_dt$lat) + 0.5 * sd(house_dt$lat),
                  scen=4)

xscen <- cfName(xscen, 'Longitude + 0.5 sd', scen=5)
xscen <- cfChange(xscen, 'long', 
                  x=mean(house_dt$long) + 0.5 * sd(house_dt$long),
                  scen=5)

xscen <- cfName(xscen, 'Sqft of Lot + 0.5 sd', scen=6)
xscen <- cfChange(xscen, 'sqft_lot15', 
                  x=mean(house_dt$sqft_lot15) + 0.5 * sd(house_dt$sqft_lot15),
                  scen=6)

xscen <- cfName(xscen, 'Avg Sqft of Room + 0.5 sd', scen=7)
xscen <- cfChange(xscen, 'room_avg_sqft', 
                  x=mean(house_dt$room_avg_sqft) + 0.5 * sd(house_dt$room_avg_sqft),
                  scen=7)

xscen <- cfName(xscen, 'Age of House + 0.5 sd', scen=8)
xscen <- cfChange(xscen, 'house_age', 
                  x=mean(house_dt$house_age) + 0.5 * sd(house_dt$house_age),
                  scen=8)

## simulate conditional expectation for counterfactuals
sims <- 10000

simbeta_lm <- mvrnorm(sims, lm2_pe, lm2_vc)
lm2_qoi <- linearsimfd(xscen, simbeta_lm, ci=0.95)

trace1 <- ropeladder(
  x=lm2_qoi$pe,
  lower=lm2_qoi$lower,
  upper=lm2_qoi$upper,
  labels=row.names(xscen$x),
  plot=1
)

singlevertmark <- linesTile(
  x=c(0,0),
  y=c(0,1),
  lty='solid',
  plot=1)

tile(
  RxC = c(1,1),
  width=list(plot=2, yaxistitle=0.5, rightborder=2),
  singlevertmark,
  trace1,
  xaxis = list(at=c(0, 0.02, 0.05, 0.1), 
               labels=c('+0', '+8.6k', '+22k', '+45k')),
  xaxistitle=list(labels='E(house price)'),
  topaxis = list(at=c(0, 0.02, 0.05, 0.1), 
                 labels=c('1x', '1.02x', '1.05x', '1.1x'),
                 add=T),
  topaxistitle = list(labels='E(house price) / average'),
  gridlines = list(type='t'),
  output = list(file='rop_lm', width=8)
)










