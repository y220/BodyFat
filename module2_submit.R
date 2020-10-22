rm(list = ls())
library(car)

# Body Fat data
BodyFat<-read.csv("BodyFat.csv", header = TRUE)
attach(BodyFat)
dim(BodyFat)  # 252 observations, 17 variables
colnames(BodyFat)
## BODYFAT-percent body fat from Siri's eqation
## DENSITY-density determined from underwater weighing (not considered as predictor)
## WEIGHT-weight(lbs)
## HEIGHT-height(cm)
## ADIPOSITY-adiposity(bmi) bmi=mass(lbs)/height^2(inch)*703
## all the remaining variables are measured in centimeters

# Analyzing raw data
head(BodyFat)
tail(BodyFat)
summary(BodyFat)
## check for abnormal data
BodyFat[BODYFAT == min(BODYFAT),]  # minimum BODYFAT is 0, delete this record
BodyFat[BODYFAT == max(BODYFAT),]
BodyFat[HEIGHT == min(HEIGHT),]
BodyFat[NECK == max(NECK),]
BodyFat[ANKLE == max(ANKLE),]

# Visualizing data
hist(BODYFAT, breaks = 30, cex.lab = 1.5, cex.main = 1.5, 
     main = "Histogram of Body Fat %", xlab = "Body Fat %")  # normal?
par(mfrow = c(1, 2))
plot(AGE, BODYFAT, xlab = "age", ylab = "body fat %", main ="body fat % vs age")
plot(WEIGHT, BODYFAT, xlab = "weight", ylab = "body fat %", main ="body fat % vs weight")
plot(HEIGHT, BODYFAT, xlab = "height", ylab = "body fat %", main = "body fat % vs height")
plot(ADIPOSITY, BODYFAT, xlab = "adiposity", ylab = "body fat %", main = "body fat % vs adiposity")
plot(NECK, BODYFAT, xlab = "neck", ylab = "body fat %", main = "body fat % vs neck")
plot(CHEST, BODYFAT, xlab = "chest", ylab = "body fat %", main = "body fat % vs chest")
plot(ABDOMEN, BODYFAT, xlab = "abdomen", ylab = "body fat %", main = "body fat % vs abdomen")
plot(HIP, BODYFAT, xlab = "hip", ylab = "body fat %", main = "body fat % vs hip")
plot(THIGH, BODYFAT, xlab = "hip", ylab = "body fat %", main = "body fat % vs thigh")
plot(KNEE, BODYFAT, xlab = "knee", ylab = "body fat %", main = "body fat % vs knee")
plot(ANKLE, BODYFAT, xlab = "ankle", ylab = "body fat %", main = "body fat % vs ankle")
plot(BICEPS, BODYFAT, xlab = "biceps", ylab = "body fat %", main = "body fat % vs biceps")
plot(FOREARM, BODYFAT, xlab = "forearm", ylab = "body fat %", main = "body fat % vs forearm")

par(mfrow = c(1, 1))
pairs(BodyFat[,-c(1, 3)])
cor(BodyFat[,-c(1, 3)])

# data pre-processing
BodyFat<-BodyFat[which(BODYFAT != 0), -c(1,3)]  # another abnormal data: BODYFAT=1.9, ADIPOSITY=20.6
summary(BodyFat)
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
## AIC
fullmod<-lm(BODYFAT~. , data = BodyFat)
step(fullmod, direction = "both", trace = 1, k =2 )  # BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST
mod0<-lm(BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST, data = BodyFat)
summary(mod0)
vif(mod0)  # hip multicollinearity

## BIC
fullmod<-lm(BODYFAT~., data = BodyFat)
step(fullmod, direction = "both", trace = 1, k = log(dim(BodyFat)[1]))  # BODYFAT ~ WEIGHT + ABDOMEN + FOREARM + WRIST
mod1<-lm(BODYFAT ~ WEIGHT + ABDOMEN + FOREARM + WRIST, data = BodyFat)
summary(mod1)
vif(mod1)  # weight multicollinearity

## only keep what we think important variable & use Mallow's cp
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

## BodyFat~AGE+ADIPOSITY (formula from wikipedia)
mod2<-lm(BODYFAT ~ AGE + ADIPOSITY, data = BodyFat)
summary(mod2)

## model selection
accuracy<-function(df, train.id, test.id){
  train<-df[train.id,]
  test<-df[test.id,]
  x.train<-as.matrix(train[,-1])
  colnames(x.train)<-NULL
  x.pc<-x.train %*% e
  colnames(x.pc)<-c("pc1", "pc2")
  train.pc<-data.frame("BODYFAT" = train[,1], x.pc)
  x.test<-as.matrix(test[,-1])
  colnames(x.test)<-NULL
  xt.pc<-x.test %*% e
  colnames(xt.pc)<-c("pc1", "pc2")
  test.pc<-data.frame("BODYFAT" = test[,1], xt.pc)
  # fit the model
  mod1<-lm(BODYFAT ~ ., data = train.pc)  # principal component regression
  mod2<-lm(BODYFAT ~ AGE + ADIPOSITY, data = train)  # existing formula
  # predict in test subset
  pred1<-predict(mod1, newdata = test.pc)  # principal component regression
  pred2<-predict(mod2, newdata = test)  # existing formula
  # accuracy (measured using mean square error)
  mse1<-mean((test[,1]-pred1)^2)
  mse2<-mean((test[,1]-pred2)^2)
  return(c(mse1, mse2))
}

n<-dim(BodyFat)[1]
tt.ratio<-0.7
n.train<-n*tt.ratio
acc.rep<-replicate(n = 3000, 
                   expr = {train.id<-sample(x = 1:n, size = n.train, replace = FALSE);
                   test.id<-seq(1, n)[-train.id]; 
                   accuracy(BodyFat, train.id, test.id)})
acc<-rowMeans(acc.rep)

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

## BodyFat~AGE+ADIPOSITY (formula from wikipedia)
reg<-lm(BODYFAT ~ AGE + ADIPOSITY, data = BodyFat)
summary(reg)
plot(reg$fitted.values, reg$residuals, xlab = "fitted values", ylab = "residuals", 
     main = "residuals vs. fitted")
## consider no outlier and equal variance
qqnorm(residuals(reg), datax = TRUE, xlab = "theoretical percentile", ylab = "residuals")
qqline(residuals(reg), datax = TRUE)
## consider normality assumption satisfied (tail thinner than normal distribution)
plot(reg, which = 4)
## no.39 potential influential point
reg.red<-lm(BODYFAT ~ AGE + ADIPOSITY, data = BodyFat[-39,])
summary(reg.red)
summary(reg)  # coefficient not changed too much
# no.39 may not be influential point
plot(reg.red$fitted.values, reg.red$residuals, xlab = "fitted values", ylab = "residuals", 
     main = "residuals vs. fitted")
## consider no outlier and equal variance
qqnorm(residuals(reg.red), datax = TRUE, xlab = "theoretical percentile", ylab = "residuals")
qqline(residuals(reg.red), datax = TRUE)
## consider normality assumption satisfied (tail thinner than normal distribution)
plot(reg.red, which = 4)

## no.39 may not be influential point
summary(reg.red)
### BODYFAT ~ -27.9714 + 0.1193*AGE + 1.6393*ADIPOSITY

## Comparison: principal component regression is more interpretable, while formula from wikipedia is quite simple
## use principal component regression as the final model

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

## what we also consider about multicollinearity
### ridge regression
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
mod1<-all.fit[[which(mse == min(mse))]]
allsubset[which(mse == min(mse)),]  # BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST
### ridge regression include too many predictors


