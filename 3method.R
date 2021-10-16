rm(list=ls())
dat=read.csv("adjust_bodyfat.csv",header=TRUE)
library(MASS)
library(tidyverse)
#delete the variable density
dat_del=dat[,c(-1,-2,-4,-6,-7)]
#=====stepwise========
full.model=lm(BODYFAT~.,data=dat_del)
step.model=stepAIC(full.model,direction = "both", trace = FALSE)
summary(step.model)
#======ridge==========
library(glmnet)

x<-dat_del[,-1]%>%as.matrix()
y<-dat_del[,1]%>%as.matrix()
fit_ridge <- glmnet(x, y, alpha=0)
cv.out <- cv.glmnet(x, y, alpha=0)
best_lamb <- cv.out$lambda.min
predict(fit_ridge, type='coefficients', s=best_lamb)

r_square<-fit_ridge$dev.ratio[which(fit_ridge$lambda==best_lamb)]
#======lasso===========

fit_lasso <- glmnet(x, y, family="gaussian", alpha=1)
plot(fit_lasso, xvar='lambda', label = TRUE)

cv.out <- cv.glmnet(x, y, alpha=1)
plot(cv.out)
best_lamb<-cv.out$lambda.min
lasso.coef <- predict(fit_lasso, type='coefficients', s=best_lamb)

r_square<-fit_lasso$dev.ratio[which(fit_lasso$lambda == best_lamb)]
