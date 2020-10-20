rm(list=ls())
library(car)
library(MASS)
library(ridge)
BodyFat <- read.csv("BodyFat.csv")
summary(BodyFat)

#Find Wrong Points: 69.42955
#Remove Wrong Points
Outlier1 = BodyFat[BodyFat$BODYFAT==0,]$IDNO
data = BodyFat[-Outlier1,]
Outlier2 = BodyFat[BodyFat$HEIGHT==min(BodyFat$HEIGHT),]$IDNO
data$HEIGHT[Outlier2]=sqrt(BodyFat$WEIGHT[Outlier2]*0.4536/BodyFat$ADIPOSITY[Outlier2])*39.3701   
# Outlier3 = BodyFat[BodyFat$WEIGHT==max(BodyFat$WEIGHT),]$IDNO
# data$WEIGHT[Outlier3]=(BodyFat$HEIGHT[Outlier2]*0.0254)^2*BodyFat$ADIPOSITY[Outlier2]/0.4536
# Outlier4 = BodyFat[BodyFat$ADIPOSITY==max(BodyFat$ADIPOSITY),]$IDNO

data =  data[,-c(1,3)]
summary(data)

kappa(cor(data),exact = TRUE)
Ridge_lm1<- lm.ridge(BODYFAT ~. , lambda = seq(0,0.3,0.001) , data = data)

Ridge_lm1
plot(Ridge_lm1)

IRid = linearRidge(BODYFAT~. , data=data)
summary(IRid)

# BODYFAT ~ AGE + NECK + ABDOMEN + THIGH + FOREARM + WRIST

IRid2 = linearRidge(BODYFAT~ AGE + NECK + ABDOMEN + THIGH + FOREARM + WRIST , data=data)
summary(IRid2)

# BODYFAT ~ AGE + NECK + ABDOMEN + FOREARM + WRIST

IRid2 = linearRidge(BODYFAT~ AGE + NECK + ABDOMEN + FOREARM + WRIST , data=data)
summary(IRid2)











