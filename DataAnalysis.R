rm(list=ls())
library(car)
library(MASS)
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

kappa(data[,2:15])


#Find the best model by AIC  
Full_Model1 <- lm(BODYFAT ~ . , data = data)
summary(Full_Model1)
slm1 <- step(Full_Model1,k = 2, direction='backward')
# slm2 <- step(Full_Model1,k = 2, direction='both')
vif(slm1)


#Find the best model by BIC 
n=nrow(data)
slm3 <- step(Full_Model1,k = log(n), direction='backward')
# slm4 <- step(Full_Model1,k = log(n), direction='both')
vif(slm3)

anova(slm1,slm3)


#Best: BODYFAT ~ DENSITY + AGE + CHEST + HIP + BICEPS
Best_Model1 <- lm(BODYFAT ~ DENSITY + AGE + CHEST + HIP + BICEPS, data = data)
summary(Best_Model1)
#Diagnostics

#Standard diagnostic plots
par(mfrow=c(2,2))
plot(Best_Model1, pch=23 ,bg='orange',cex=2)

#Internally studentized residuals and Externally studentized residuals
par(mfrow=c(1,1))
plot(resid(Best_Model1), rstudent(Best_Model1), pch=24, bg='blue', cex=1.5)
plot(rstandard(Best_Model1), rstudent(Best_Model1), pch=24, bg='blue', cex=1.5)
qqnorm(rstandard(Best_Model1), pch=24, bg='red', cex=1.5)
plot(fitted(Best_Model1), sqrt(abs(rstandard(Best_Model1))), 
     pch=23, bg='red', ylim=c(0,1))
plot(fitted(Best_Model1), resid(Best_Model1), pch=23, bg='red', cex=2)
abline(h=0, lty=2)


#DFFITS
plot(dffits(Best_Model1), pch=23, bg='orange', cex=2, ylab="DFFITS")
data[which(dffits(Best_Model1) > 0.5),] # 76 96

#Cook's distance
plot(cooks.distance(Best_Model1), 
     pch=24, bg='orange', cex=1, ylab="Cook's distance")
data[which(cooks.distance(Best_Model1) > 0.1),] # 48 96

# DFBETAS
# plot(dfbetas(Best_Model1), pch=23, bg='orange', cex=2, ylab="DFBETA (Climb)")
# data[which(abs(dfbetas(Best_Model1)) > 1),]
# outlierTest(Best_Model1)


# Refit
Best_Model2 <- lm(BODYFAT ~ DENSITY + AGE + CHEST + HIP + BICEPS,
                  data = data[-c(48,96),])
summary(Best_Model2)

#add variables 
#avPlots(Best_Model1, '')
vif(Best_Model1)


