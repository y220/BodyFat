## Contributions
## MC created the code and proposed modelling part. 
## YF proposed ideas model selection and model diagnostic method. 


rm(list = ls())
library(car)
library(xtable)

BodyFat<-read.csv("BodyFat.csv", header = TRUE)
attach(BodyFat)
dim(BodyFat)  # 252 observations, 17 variables
colnames(BodyFat)


# data pre-processing
BodyFat<-BodyFat[which(BODYFAT != 0), -c(1,3)]
summary(BodyFat)  # another abnormal data: BODYFAT=1.9, ADIPOSITY=20.6
BodyFat<-BodyFat[which(BodyFat$BODYFAT > min(BodyFat$BODYFAT)),]
summary(BodyFat)
## weight-height-bmi
bmi<-BodyFat[,3:5]
bmi[abs(bmi$ADIPOSITY-bmi$WEIGHT*703/bmi$HEIGHT^2) > 1,]
### 42: 205.00-29.50-29.9
### 163: 184.25-68.75-24.4  bmi (27.17,27.64)
### 221: 153.25-70.5-24.5  bmi (21.65,21.87)
BodyFat[42,4]<-sqrt(BodyFat[42,3]*703/BodyFat[42,5])
summary(BodyFat)

# modelling
## principal component regression
Sigma<-cov(BodyFat[,-1])
eigen.val<-eigen(Sigma)$values
eigen.vec<-eigen(Sigma)$vectors

Sigma<-cov(BodyFat[,-c(1,6,11,12,13,14,15)])
eigen.val<-eigen(Sigma)$values
eigen.vec<-eigen(Sigma)$vectors
cum.var<-cumsum(eigen.val)/sum(eigen.val)
plot(1:8, cum.var)  # use first two principal components
e<-eigen.vec[,1:3]
x<-as.matrix(BodyFat[,-c(1,6,11,12,13,14,15)])
colnames(x)<-NULL
pc<-x %*% e
colnames(pc)<-c("pc1", "pc2", "pc3")
bodyfat<-data.frame("bodyfat" = BodyFat[,1], pc)
mod<-lm(bodyfat~., data = bodyfat)
summary(mod)


# model diagnosis: principal component regression
## principal component
print(e)
### pc1 = 0.0036*AGE + 0.8859*WEIGHT + 0.0342*HEIGHT + 0.1004*ADIPOSITY + 0.2325*CHEST +
###       0.2979*ABDOMEN + 0.2055*HIP + 0.1374*THIGH
### pc2 = 0.9619*AGE - 0.0812*WEIGHT - 0.0615*HEIGHT + 0.0352*ADIPOSITY + 0.1189*CHEST +
###       0.1989*ABDOMEN - 0.0371*HIP - 0.0895*THIGH
### pc3 = -0.2336*AGE - 0.3794*WEIGHT - 0.3660*HEIGHT + 0.2281*ADIPOSITY + 0.3759*CHEST +
###       0.6685*ABDOMEN + 0.1184*HIP + 0.1124*THIGH
plot(mod$fitted.values, mod$residuals, xlab = "fitted values", ylab = "residuals", 
     main = "residuals vs. fitted")
which(mod$fitted.values == max(mod$fitted.values))
points(mod$fitted.values[39], mod$residuals[39], pch = 19, col = 2)
text(mod$fitted.values[39], mod$residuals[39], labels = "39", pos = 2)
## potential outlier: no.39

qqnorm(residuals(mod), datax = TRUE, xlab = "theoretical percentile", ylab = "residuals")
qqline(residuals(mod), datax = TRUE)
## consider normality assumption satisfied (left tail thinner than normal distribution)

plot(mod, which = 4)
## potential influential point: no.39
BodyFat[39,]

## drop no.39
bodyfat.red<-BodyFat[-39,]
Sigma.red<-cov(bodyfat.red[,-c(1,6,11,12,13,14,15)])
eigen.val.red<-eigen(Sigma.red)$values
eigen.vec.red<-eigen(Sigma.red)$vectors
cum.var.red<-cumsum(eigen.val.red)/sum(eigen.val.red)
plot(1:8, cum.var.red)  # use first two principal components
e.red<-eigen.vec.red[,1:3]
print(e.red)
### pc1 = 0.0038*AGE + 0.8826*WEIGHT + 0.0387*HEIGHT + 0.0978*ADIPOSITY + 0.2436*CHEST +
###       0.3028*ABDOMEN + 0.2001*HIP + 0.1374*THIGH
### pc2 = 0.9621*AGE - 0.0832*WEIGHT - 0.0619*HEIGHT + 0.0351*ADIPOSITY + 0.1175*CHEST +
###       0.1979*ABDOMEN - 0.0372*HIP - 0.08959*THIGH
### pc3 = -0.2304*AGE - 0.3834*WEIGHT - 0.3781*HEIGHT + 0.2347*ADIPOSITY + 0.3545*CHEST +
###       0.6670*ABDOMEN + 0.1323*HIP + 0.1173*THIGH

x.red<-as.matrix(bodyfat.red[,-c(1,6,11,12,13,14,15)])
colnames(x.red)<-NULL
pc.red<-x.red %*% e.red
colnames(pc.red)<-c("pc1", "pc2", "pc3")
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
### BODYFAT ~ -41.3962 + 0.1627*pc1 + 0.1828*pc2 + 0.6809*pc3


# make model simplier (drop outlier no.39)
df<-bodyfat.red[,-c(6,11,12,13,14,15)]
e<-round(e.red, digits = 2)
### pc1 = 0.00*AGE + 0.88*WEIGHT + 0.04*HEIGHT + 0.10*ADIPOSITY + 0.24*CHEST +
###       0.30*ABDOMEN + 0.20*HIP + 0.14*THIGH
### pc2 = 0.96*AGE - 0.08*WEIGHT - 0.06*HEIGHT + 0.04*ADIPOSITY + 0.12*CHEST +
###       0.20*ABDOMEN - 0.04*HIP - 0.09*THIGH
### pc3 = -0.23*AGE - 0.38*WEIGHT - 0.38*HEIGHT + 0.23*ADIPOSITY + 0.35*CHEST +
###       0.67*ABDOMEN + 0.13*HIP + 0.12*THIGH
x<-as.matrix(df[,-1])
colnames(x)<-NULL
pc<-x %*% e
colnames(pc)<-c("pc1", "pc2", "pc3")
df.pc<-data.frame("bodyfat" = df[,1], pc)
reg<-lm(bodyfat ~ ., data = df.pc)
summary(reg)

plot(reg$fitted.values, reg$residuals, xlab = "fitted values", ylab = "residuals", 
     main = "residuals vs. fitted")
## consider no outlier and equal variance

qqnorm(residuals(reg), datax = TRUE, xlab = "theoretical percentile", ylab = "residuals")
qqline(residuals(reg), datax = TRUE)
## consider normality assumption satisfied (left tail thinner than normal distribution)

plot(reg, which = 4)

## consider no influential point
## BODYFAT ~ -41.1569 + 0.1611*pc1 + 0.1738*pc2 + 0.6827*pc3
para<-reg$coefficients[2:4]
coef<-round(e %*% para, digits = 2)
## BODYFAT ~ -41.1569 + 0.02*AGE - 0.13*WEIGHT -0.26*HEIGHT + 0.19*ADIPOSITY +0.30*CHEST
##          +0.54*ABDOMEN + 0.11*HIP + 0.09*THIGH
vif(reg)



# appendix (other methods we tried)
## AIC
fullmod<-lm(BODYFAT~. , data = BodyFat)
step(fullmod, direction = "both", trace = 1, k =2 )  # BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST
mod0<-lm(BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST, data = BodyFat)
summary(mod0)
vif(mod0)  # abdomen multicollinearity

## BIC
fullmod<-lm(BODYFAT~., data = BodyFat)
step(fullmod, direction = "both", trace = 1, k = log(dim(BodyFat)[1]))  # BODYFAT ~ WEIGHT + ABDOMEN + FOREARM + WRIST
mod1<-lm(BODYFAT ~ WEIGHT + ABDOMEN + FOREARM + WRIST, data = BodyFat)
summary(mod1)
vif(mod1)  # weight multicollinearity

## only keep important variable & use Mallow's cp
sub.bf<-BodyFat[,c(1, 5, 7, 8, 9, 10)]
fitall<-lm(BODYFAT~., data = sub.bf)
sigma2<-summary(fitall)$sigma^2
allsubset<-as.matrix(expand.grid("ADIPOSITY" = 0:1, "CHEST" = 0:1, "ABDOMEN" = 0:1, "HIP" = 0:1, "THIGH" = 0:1))[-1,]
var.name<-c("ADIPOSITY", "CHEST", "ABDOMEN", "HIP", "THIGH")
sse<-apply(allsubset, MARGIN = 1, 
           FUN = function(ind) {var.sub<-var.name[which(ind == 1)]; 
           f<-formula(paste("BODYFAT", "~", paste(var.sub, collapse = "+")));
           reg<-lm(f, data = BodyFat);
           return(sum(residuals(reg)^2))})
n<-dim(BodyFat)[1]
cp<-sse/sigma2-n+2*rowSums(allsubset)
cp.table<-cbind(allsubset, "p" = rowSums(allsubset), "cp" = round(cp, digits = 4))   # BODYFAT ~ CHEST + ABDOMEN + HIP
mod2<-lm(BODYFAT ~ CHEST + ABDOMEN + HIP, data = BodyFat)
summary(mod2)
vif(mod2)  # ABDOMEN multicollinearity

## BodyFat~AGE+ADIPOSITY (formula from wikipedia)
### cited from https://en.wikipedia.org/wiki/Body_fat_percentage
mod3<-lm(BODYFAT ~ AGE + ADIPOSITY, data = BodyFat)
summary(mod3)

## ridge regression
ridge<-function(df, lambda){
  n<-nrow(df)
  x<-cbind(rep(1, n), as.matrix(df[, -1]))
  y<-as.matrix(df[,1])
  p<-ncol(df)-1
  coef<-apply(as.matrix(lambda, ncol = 1), MARGIN = 1, 
              FUN = function(l) {return(solve(t(x)%*%x + l*diag(x = 1, nrow = p+1, ncol = p+1)) %*% t(x) %*% y)})
  mse<-apply(coef, MARGIN = 2, 
             FUN = function(c){y.pred<-x %*% c; 
             return(sum((y-y.pred)^2)/(n-p))})
  idx<-which(mse == min(mse))
  parmse<-c(lambda[idx], mse[idx])
  coefficient<-coef[,idx]
  return(list("par" = parmse, "coef" = coefficient))
}

lambda<-seq(0.1, 5, by = 0.1)
allsubset<-as.matrix(expand.grid("AGE" = 0:1, "WEIGHT" = 0:1, "HEIGHT" = 0:1, 
                                 "ADIPOSITY" = 0:1, "NECK" = 0:1, "CHEST" = 0:1, 
                                 "ABDOMEN" = 0:1, "HIP" = 0:1, "THIGH" = 0:1, 
                                 "KNEE" = 0:1, "ANKLE" = 0:1, "BICEPS" = 0:1, 
                                 "FOREARM" = 0:1, "WRIST" = 0:1))[-1,]
var.name<-colnames(BodyFat)[-1]
all.fit<-apply(allsubset, MARGIN = 1, 
               FUN = function(ind){var.sub<-var.name[which(ind == 1)]; 
               f<-formula(paste("BODYFAT", "~", paste(var.sub, collapse = "+"))); 
               df<-BodyFat[,c(1, (which(ind == 1)+1))]; 
               out<-ridge(df, lambda); 
               return(out)})
mse<-unlist(lapply(all.fit, FUN = function(l){return(l$par[2])}))
mod4<-all.fit[[which(mse == min(mse))]]
allsubset[which(mse == min(mse)),]
### BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST
### ridge regression stil include too many predictors

detach(BodyFat)

## model selection
accuracy<-function(df, train.id, test.id){
  train<-df[train.id,]
  test<-df[test.id,]
  x.train<-as.matrix(train[,-c(1,6,11,12,13,14,15)])
  colnames(x.train)<-NULL
  Sigma<-cov(train[,-c(1,6,11,12,13,14,15)])
  eigen.val<-eigen(Sigma)$values
  eigen.vec<-eigen(Sigma)$vectors
  e<-eigen.vec[,1:3]
  x.pc<-x.train %*% e
  colnames(x.pc)<-c("pc1", "pc2", "pc3")
  train.pc<-data.frame("BODYFAT" = train[,1], x.pc)
  x.test<-as.matrix(test[,-c(1,6,11,12,13,14,15)])
  colnames(x.test)<-NULL
  xt.pc<-x.test %*% e
  colnames(xt.pc)<-c("pc1", "pc2", "pc3")
  test.pc<-data.frame("BODYFAT" = test[,1], xt.pc)
  # fit the model
  mod1<-lm(BODYFAT ~ WEIGHT + ABDOMEN + FOREARM + WRIST, data = train)  # select BIC
  mod2<-lm(BODYFAT ~ ., data = train.pc)  # principal component regression
  mod3<-lm(BODYFAT ~ AGE + ADIPOSITY, data = train)  # existing formula
  # predict in test subset
  pred1<-predict(mod1, newdata = test)
  pred2<-predict(mod2, newdata = test.pc)  # principal component regression
  pred3<-predict(mod3, newdata = test)  # existing formula
  # accuracy (measured using mean square error)
  rmse1<-mean((test[,1]-pred1)^2)
  rmse2<-mean((test[,1]-pred2)^2)
  rmse3<-mean((test[,1]-pred3)^2)
  return(c(rmse1, rmse2, rmse3))
}

n<-dim(BodyFat)[1]
tt.ratio<-0.7
n.train<-n*tt.ratio
acc.rep<-replicate(n = 5000, 
                   expr = {train.id<-sample(x = 1:n, size = n.train, replace = FALSE);
                   test.id<-seq(1, n)[-train.id]; 
                   accuracy(BodyFat, train.id, test.id)})
acc<-rowMeans(acc.rep)
