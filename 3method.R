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
#=======lasso===========
fit_lasso <- glmnet(x, y, family="gaussian", alpha=1)
plot(fit_lasso, xvar='lambda', label = TRUE)

cv.out <- cv.glmnet(x, y, alpha=1)
plot(cv.out)
best_lamb<-cv.out$lambda.min
predict(fit_lasso, type='coefficients', s=best_lamb)
fit_lasso$dev.ratio[which(fit_lasso$lambda == best_lamb)]

#=======random forest=========
library(randomForest)
fit_rf<-randomForest(BODYFAT~., data = dat_del)
summary(fit_rf)
importance(fit_rf)

#=========split in age========
dat_del$age_group<-'g2'
dat_del$age_group[which(dat$AGE<=40)]<-'g1'


dat_del$age_group<-'g2'
dat_del$age_group[which(dat$AGE<=40)]<-'g1'
dat_del$age_group[which(dat$AGE>60)]<-'g3'



boxplot(BODYFAT~age_group,data = dat_del)

plot(BODYFAT~factor(age_group)*factor(ADIPOSITY),data=dat_del)

#======age group under 40======
sub<-which(dat_del$age_group=='g1')
fit<-lm(BODYFAT~.,dat_del[sub,-16])
step(fit)
#Stepwise model
fit<-lm(BODYFAT ~ ABDOMEN + HIP + THIGH + WRIST,dat=dat_del[sub,])
summary(fit)

#=====age group under 60=====
sub<-which(dat_del$age_group=='g2')
fit<-lm(BODYFAT~.,dat_del[sub,-16])
step(fit)
#Stepwise model
fit<-lm(BODYFAT ~  AGE + NECK + ABDOMEN + ANKLE + FOREARM + WRIST,dat=dat_del[sub,])
summary(fit)
#drop not significant
fit<-lm(BODYFAT ~  NECK + ABDOMEN + ANKLE + FOREARM + WRIST,dat=dat_del[sub,])

summary(fit)

#=====age group above 60=====
sub<-which(dat_del$age_group=='g3')
fit<-lm(BODYFAT~.,dat_del[sub,-16])
step(fit)
#Stepwise model
fit<-lm(BODYFAT ~ ABDOMEN + ANKLE + BICEPS + FOREARM,dat=dat_del[sub,])
summary(fit)
#drop not significant
fit<-lm(BODYFAT ~ ABDOMEN + ANKLE ,dat=dat_del[sub,])
summary(fit)



#=========poly==========
#ABDOMEN/HIP
#ADIPOSITY^2



