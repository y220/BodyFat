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
BodyFat<-BodyFat[which(BodyFat$BODYFAT > min(BodyFat$BODYFAT)),]
## weight-height-bmi
bmi<-BodyFat[,5:7]
bmi[abs(bmi$ADIPOSITY-bmi$WEIGHT*703/bmi$HEIGHT^2) > 1,]
### 42: 205.00-29.50-29.9
### 163: 184.25-68.75-24.4  bmi (27.17,27.64)
### 221: 153.25-70.5-24.5  bmi (21.65,21.87)
BodyFat[42,6]<-sqrt(BodyFat[42,5]*703/BodyFat[42,7])
summary(BodyFat)

# modelling
## AIC
fullmod<-lm(BODYFAT~. , data = BodyFat)
step(fullmod, direction = "both", trace = 1, k =2 )  # BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST
mod0<-lm(BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST, data = BodyFat)
summary(mod0)
vif(mod0)  # weight multicollinearity

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

## using above two methods for model selection cause heavy multicollinearity

## principal component/ridge regression
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
mod2<-lm(bodyfat~. , data = bodyfat)
summary(mod2)

## BodyFat~AGE+ADIPOSITY (formula from wikipedia)
mod3<-lm(BODYFAT ~ AGE + ADIPOSITY, data = BodyFat)
summary(mod3)

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
  mod1<-ridge(train[,c(1,2,3,6,8,9,10,14,15)], lambda)  # ridge regression
  mod2<-lm(BODYFAT ~ ., data = train.pc)  # principal component regression
  mod3<-lm(BODYFAT ~ AGE + ADIPOSITY, data = train)  # existing formula
  # predict in test subset
  x.mat<-cbind(rep(1, nrow(test)), as.matrix(test[,c(2,3,6,8,9,10,14,15)]))
  pred1<-x.mat %*% mod1$coef  # ridge regression
  pred2<-predict(mod2, newdata = test.pc)  # principal component regression
  pred3<-predict(mod3, newdata = test)  # existing formula
  # accuracy (measured using mean square error)
  mse1<-mean((test[,1]-pred1)^2)
  mse2<-mean((test[,1]-pred2)^2)
  mse3<-mean((test[,1]-pred3)^2)
  return(c(mse1, mse2, mse3)) 
}

tt.ratio<-0.7
n.train<-n*tt.ratio
acc.rep<-replicate(n = 3000, 
               expr = {train.id<-sample(x = 1:n, size = n.train, replace = FALSE);
               test.id<-seq(1, n)[-train.id]; 
               accuracy(BodyFat, train.id, test.id)})
acc<-rowMeans(acc.rep)

# model: BODYFAT ~ AGE + WEIGHT + NECK + ABDOMEN + HIP + THIGH + FOREARM + WRIST
reg<-ridge(BodyFat[,c(1,2,3,6,8,9,10,14,15)], lambda)
reg$par
reg$coef
## BODYFAT ~ -11.9512 + 0.0485 * AGE - 0.0666 * WEIGHT - 0.2892 * NECK + 0.8765 * ABDOMEN -0.2531 * HIP + 0.2297 * THIGH + 0.4028 * FOREARM - 1.6110 * WRIST
x.val<-as.matrix(BodyFat[,c(2,3,6,8,9,10,14,15)])
y.fit<-cbind(rep(1, nrow(x.val)), x.val) %*% reg$coef
res<-BodyFat[,1]-y.fit
plot(y.fit, res, xlab = "fit", ylab = "residual", main = "fitted vs. residual")
## considered no outliers and equal variance
qqnorm(res, datax = TRUE, ylab = "residual", xlab = "quantile")
qqline(res, datax = TRUE)
## consider normality assumption satisfied (tails thinner than normal distribution)


