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
cum.var<-cumsum(eigen.val)/sum(eigen.val)
plot(1:14, cum.var)  # use first two principal components
e<-eigen.vec[,1:2]
x<-as.matrix(BodyFat[,-1])
colnames(x)<-NULL
pc<-x %*% e
colnames(pc)<-c("pc1", "pc2")
bodyfat<-data.frame("bodyfat" = BodyFat[,1], pc)
mod<-lm(bodyfat~., data = bodyfat)
summary(mod)



# model diagnosis: principal component regression
## principal component
print(e)
### pc1 = 0.0035*AGE + 0.8792*WEIGHT + 0.0340*HEIGHT + 0.0996*ADIPOSITY + 0.0606*NECK +
###       0.2307*CHEST + 0.2953*ABDOMEN + 0.2038*HIP + 0.1364*THIGH + 0.0612*KNEE +
###       0.0304*ANKLE + 0.0720*BICEPS + 0.0374*FOREARM + 0.0201*WRIST
### pc2 = 0.9612*AGE - 0.0801*WEIGHT - 0.0614*HEIGHT + 0.0353*ADIPOSITY + 0.0178*NECK +
###       0.1191*CHEST + 0.1994*ABDOMEN - 0.0368*HIP - 0.0894*THIGH - 0.0029*KNEE -
###       0.0175*ANKLE - 0.0151*BICEPS - 0.0164*FOREARM + 0.0129*WRIST

plot(mod$fitted.values, mod$residuals, xlab = "fitted values", ylab = "residuals", 
     main = "residuals vs. fitted")
which(mod$fitted.values == max(mod$fitted.values))
## potential outlier: no.39

qqnorm(residuals(mod), datax = TRUE, xlab = "theoretical percentile", ylab = "residuals")
qqline(residuals(mod), datax = TRUE)
## consider normality assumption satisfied (left tail thinner than normal distribution)

plot(mod, which = 4)
## potential influential point: no.39
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
### pc1 = 0.0036*AGE + 0.8756*WEIGHT + 0.0385*HEIGHT + 0.0970*ADIPOSITY + 0.0599*NECK + 
###       0.2415*CHEST + 0.3000*ABDOMEN + 0.1984*HIP + 0.1364*THIGH + 0.0630*KNEE +
###       0.0302*ANKLE + 0.0738*BICEPS + 0.0440*FOREARM + 0.0210*WRIST
### pc2 = 0.9614*AGE - 0.0818*WEIGHT - 0.0618*HEIGHT + 0.0352*ADIPOSITY + 0.0177*NECK + 
###       0.1179*CHEST + 0.1984*ABDOMEN - 0.0369*HIP - 0.0897*THIGH - 0.0032*KNEE -
###       0.0176*ANKLE - 0.0154*BICEPS - 0.0169*FOREARM + 0.0128*WRIST
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



# make model simplier (drop outlier no.39)
## only keep those relative important predictors: AGE, WEIGHT, CHEST, ABDOMEN, HIP, THIGH
### pc1 = 0.0036*AGE + 0.8756*WEIGHT + 0.0385*HEIGHT + 0.0970*ADIPOSITY + 0.0599*NECK + 
###       0.2415*CHEST + 0.3000*ABDOMEN + 0.1984*HIP + 0.1364*THIGH + 0.0630*KNEE +
###       0.0302*ANKLE + 0.0738*BICEPS + 0.0440*FOREARM + 0.0210*WRIST
### pc2 = 0.9614*AGE - 0.0818*WEIGHT - 0.0618*HEIGHT + 0.0352*ADIPOSITY + 0.0177*NECK + 
###       0.1179*CHEST + 0.1984*ABDOMEN - 0.0369*HIP - 0.0897*THIGH - 0.0032*KNEE -
###       0.0176*ANKLE - 0.0154*BICEPS - 0.0169*FOREARM + 0.0128*WRIST
df<-bodyfat.red[,c(1,2,3,7,8,9,10)]
e<-matrix(c(0, 0.88, 0.24, 0.30, 0.20, 0.14, 
            0.96, 0, 0, 0, 0, 0), 
          byrow = FALSE, ncol = 2, nrow = 6)
x<-as.matrix(df[,-1])
colnames(x)<-NULL
pc<-x %*% e
colnames(pc)<-c("pc1", "pc2")
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
## BODYFAT ~ -27.5858 + 0.1644*pc1 + 0.1759*pc2
## pc1 ~ 0.88*WEIGHT + 0.24*CHEST + 0.30*ABDOMEN + 0.20*HIP + 0.14*THIGH
## pc2 ~ 0.96*AGE
vif(reg)



# appendix (other methods we tried)
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
