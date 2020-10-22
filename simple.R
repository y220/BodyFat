rm(list = ls())
library(car)

BodyFat<-read.csv("BodyFat.csv", header = TRUE)

# data pre-processing
BodyFat<-BodyFat[which(BODYFAT != 0), -c(1,3)]  # another abnormal data: BODYFAT=1.9, ADIPOSITY=20.6
BodyFat<-BodyFat[which(BodyFat$BODYFAT > min(BodyFat$BODYFAT)),]
## weight-height-bmi
bmi<-BodyFat[,3:5]
bmi[abs(bmi$ADIPOSITY-bmi$WEIGHT*703/bmi$HEIGHT^2) > 1,]
### 42: 205.00-29.50-29.9
### 163: 184.25-68.75-24.4  bmi (27.17,27.64)
### 221: 153.25-70.5-24.5  bmi (21.65,21.87)
BodyFat[42,4]<-sqrt(BodyFat[42,3]*703/BodyFat[42,5])

### principal component
Sigma<-cov(BodyFat[,-1])
eigen.val<-eigen(Sigma)$values
eigen.vec<-eigen(Sigma)$vectors
cum.var<-cumsum(eigen.val)/sum(eigen.val)
plot(1:14, cum.var)  # use first two principal components
e<-eigen.vec[,1:2]
x<-as.matrix(BodyFat[,-1])
colnames(x)<-NULL
pc<-x %*% e
colnames(pc)<-c("pc1", "pc2")
bodyfat<-data.frame("bodyfat" = BodyFat[,1], pc)
mod1<-lm(bodyfat~., data = bodyfat)
summary(mod1)


# model diagnosis: principal component regression
## principal component
Sigma<-cov(BodyFat[,-1])
eigen.val<-eigen(Sigma)$values
eigen.vec<-eigen(Sigma)$vectors
cum.var<-cumsum(eigen.val)/sum(eigen.val)
plot(1:14, cum.var)  # use first two principal components
e<-eigen.vec[,1:2]
print(e)
## pc1 = 0.0035*AGE + 0.8795*WEIGHT + 0.0296*HEIGHT + 0.0997*ADIPOSITY + 0.0582*NECK + 
##       0.2308*CHEST + 0.2954*ABDOMEN + 0.2038*HIP + 0.1364*THIGH + 0.0612*KNEE +
##       0.0304*ANKLE + 0.0720*BICEPS + 0.0374*FOREARM + 0.0202*WRIST
## pc2 = 0.9612*AGE - 0.0805*WEIGHT - 0.0621*HEIGHT + 0.0353*ADIPOSITY + 0.0173*NECK + 
##       0.1190*CHEST + 0.1995*ABDOMEN - 0.0364*HIP - 0.0890*THIGH - 0.0028*KNEE -
##       0.0176*ANKLE - 0.0152*BICEPS - 0.0164*FOREARM + 0.0128*WRIST
x<-as.matrix(BodyFat[,-1])
colnames(x)<-NULL
pc<-x %*% e
colnames(pc)<-c("pc1", "pc2")
bodyfat<-data.frame("bodyfat" = BodyFat[,1], pc)
mod<-lm(bodyfat~., data = bodyfat)
summary(mod)

plot(mod$fitted.values, mod$residuals, xlab = "fitted values", ylab = "residuals", 
     main = "residuals vs. fitted")
which(mod$fitted.values == max(mod$fitted.values))
## potential one outlier: no.39
qqnorm(residuals(mod), datax = TRUE, xlab = "theoretical percentile", ylab = "residuals")
qqline(residuals(mod), datax = TRUE)
## consider normality assumption satisfied (left tail thinner than normal distribution)
plot(mod, which = 4)
## potential influential point: 39
BodyFat[39,]
## drop no.39
bodyfat.red<-BodyFat[-39,]
Sigma.red<-cov(bodyfat.red[,-1])
eigen.val.red<-eigen(Sigma.red)$values
eigen.vec.red<-eigen(Sigma.red)$vectors
cum.var.red<-cumsum(eigen.val.red)/sum(eigen.val.red)
plot(1:14, cum.var.red)  # use first two principal components
e.red<-eigen.vec.red[,1:2]
print(e.red)
## pc1 = 0.0036*AGE + 0.8758*WEIGHT + 0.0331*HEIGHT + 0.0970*ADIPOSITY + 0.0570*NECK + 
##       0.2417*CHEST + 0.30001*ABDOMEN + 0.1983*HIP + 0.1364*THIGH + 0.0630*KNEE +
##       0.0302*ANKLE + 0.0738*BICEPS + 0.0441*FOREARM + 0.0210*WRIST
## pc2 = 0.9614*AGE - 0.0823*WEIGHT - 0.0624*HEIGHT + 0.0353*ADIPOSITY + 0.0172*NECK + 
##       0.1177*CHEST + 0.1985*ABDOMEN - 0.0365*HIP - 0.0894*THIGH - 0.0031*KNEE -
##       0.0176*ANKLE - 0.0155*BICEPS - 0.0170*FOREARM + 0.0127*WRIST
x.red<-as.matrix(bodyfat.red[,-1])
colnames(x.red)<-NULL
pc.red<-x.red %*% e.red
colnames(pc.red)<-c("pc1", "pc2")
bodyfat.pc<-data.frame("bodyfat" = bodyfat.red[,1], pc.red)
mod.red<-lm(bodyfat ~ ., data = bodyfat.pc)
summary(mod.red)
summary(mod)  # coefficient not changed too much
## no.39 may not be influential point
plot(mod.red$fitted.values, mod.red$residuals, xlab = "fitted values", ylab = "residuals", 
     main = "residuals vs. fitted")
## consider no outlier and equal variance
qqnorm(residuals(mod.red), datax = TRUE, xlab = "theoretical percentile", ylab = "residuals")
qqline(residuals(mod.red), datax = TRUE)
## consider normality assumption satisfied (left tail thinner than normal distribution)
plot(mod.red, which = 4)

## no.39 may not be influential point
summary(mod.red)
### BODYFAT ~ -29.7950 + 0.1610*pc1 + 0.1829*pc2

# make model more simplier
bodyfat.red<-BodyFat[-39,]
Sigma.red<-cov(bodyfat.red[,-1])
eigen.val.red<-eigen(Sigma.red)$values
eigen.vec.red<-eigen(Sigma.red)$vectors
cum.var.red<-cumsum(eigen.val.red)/sum(eigen.val.red)
plot(1:14, cum.var.red)  # use first two principal components
e.red<-eigen.vec.red[,1:2]
print(e.red)
## pc1 = 0.0036*AGE + 0.8758*WEIGHT + 0.0331*HEIGHT + 0.0970*ADIPOSITY + 0.0570*NECK + 
##       0.2417*CHEST + 0.3000*ABDOMEN + 0.1983*HIP + 0.1364*THIGH + 0.0630*KNEE +
##       0.0302*ANKLE + 0.0738*BICEPS + 0.0441*FOREARM + 0.0210*WRIST
## pc2 = 0.9614*AGE - 0.0823*WEIGHT - 0.0624*HEIGHT + 0.0353*ADIPOSITY + 0.0172*NECK + 
##       0.1177*CHEST + 0.1985*ABDOMEN - 0.0365*HIP - 0.0894*THIGH - 0.0031*KNEE -
##       0.0176*ANKLE - 0.0155*BICEPS - 0.0170*FOREARM + 0.0127*WRIST
## only keep those relative important predictors: AGE, WEIGHT, CHEST, ABDOMEN, HIP, THIGH
df<-bodyfat.red[,c(1,2,3,7,8,9,10)]
e<-matrix(c(0, 0.88, 0.24, 0.30, 0.20, 0.14, 0.96, 0, 0, 0, 0, 0), 
          byrow = FALSE, ncol = 2, nrow = 6)
x<-as.matrix(df[,-1])
colnames(x)<-NULL
pc<-x %*% e
colnames(pc)<-c("pc1", "pc2")
df.pc<-data.frame("bodyfat" = df[,1], pc)
mod<-lm(bodyfat ~ ., data = df.pc)
summary(mod)
plot(mod$fitted.values, mod$residuals, xlab = "fitted values", ylab = "residuals", 
     main = "residuals vs. fitted")
## consider no outlier and equal variance
qqnorm(residuals(mod), datax = TRUE, xlab = "theoretical percentile", ylab = "residuals")
qqline(residuals(mod), datax = TRUE)
## consider normality assumption satisfied (left tail thinner than normal distribution)
plot(mod, which = 4)
### BODYFAT ~ -27.5858 + 0.1644*pc1 + 0.1759*pc2
### pc1 ~ 0.88*WEIGHT + 0.24*CHEST + 0.30*ABDOMEN + 0.20*HIP + 0.14*THIGH
### pc2 ~ 0.96*AGE
vif(mod)



