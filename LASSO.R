library(corrplot)
library(ggplot2)
library(glmnet)
library(MASS)
library(Amelia)
library(GGally)
library(car)
library(VIF)
library(psych)

lasso <- read.csv("LogCountPerformance.csv", header = T)
head(lasso)
x = as.matrix(lasso[,2:24])
y <-lasso$eyeglasscaptureratebyordernumerator
y <- lasso[1]
y <- as.matrix(lasso[1])
head(x)
head(y)#A summary of the glmnet path at each step:
Y <- lasso[1]

fit = glmnet(x, y, family = "poisson")
plot(fit)

print(fit)
fit1 = glmnet(x, y)
coef(fit, s = .004416)
coef(fit1, s = 0.2)
predict(fit, newx = x[1:860,], type = "response", s=c(0.1,1))

cvfit = cv.glmnet(x, y, family="poisson")
plot(cvfit)

cvfit1 = cv.glmnet(x, y)
plot(cvfit1)

plot(fit, label = T)


cvfit$lambda.min #0.0008182936
coef(cvfit, s="lambda.min") #19 variables

cvfit$lambda.1se #0.01934846
coef(cvfit, s="lambda.1se") #11 variables left, error within 1 standard error of the minimum


#opt.lam = c(cvfit$lambda.min, cvfit$lambda.1se)
#coef(cvfit, s= opt.lam)

R2(fit)

library(covTest)
library(RPtests)
lm.lasso <- l1ce(Y ~ ., data=lasso)
summary(lm.lasso)$coefficients

covTest(fit, x, y, sigma.est = "full", status = NULL)
a=lars(x,y)
a1=lars.glm(x,y,family="poisson")
covTest(a,x,y)

pval(x, test_sim)
out <- RPtest(x, y, test="nonlin", B=9L, nperms=2, resid_type = "Lasso")

RPtest(x, y, resid_type = "Lasso", test = c("nonlin", "group",
                                                      "hetero"), x_alt, RPfunction = NULL, B = 49L, rand_gen = rnorm,
       noise_matrix = NULL, mc.cores = 1L, nfolds = 10L, nperms = 2L,
       beta_est = NULL, resid_only = FALSE, output_all = T,
       verbose = T)
RPtest_single(x, y, B = 100L, rand_gen = rnorm, mc.cores = 1L)

s = sample(1:4298, 3438)  
eyecapture_TRAIN = x[s, ]
e = y[s, ]
xTest = x[-s, ]
yTest = y[-s, ]

#specific lambda
c = as.matrix(coef(fit, s=.004416)) #3.997743
yHat = predict(fit, xTrain, s=.003154)
dof = length(yTrain) - length(c)
rse = sqrt(sum(yTrain - yHat)^2) / dof
rse

yPredict = predict(fit, xTest, s=.003154) #4.030222
dof = length(yTest) - length(c)
rse = sqrt(sum(yTest - yPredict)^2) / dof
rse

#lambda min & lambda 1se
c = as.matrix(coef(cvfitTrain, s=.0013)) #3.997743
yHat = predict(fit, xTrain, s="lambda.1se")
dof = length(yTrain) - length(c)
rse = sqrt(sum(yTrain - yHat)^2) / dof
rse

yPredict = predict(fit, xTest, s="lambda.1se") #4.030222
dof = length(yTest) - length(c)
rse = sqrt(sum(yTest - yPredict)^2) / dof
rse

cvfitTrain = cv.glmnet(xTrain, yTrain)
plot(cvfitTrain)

print(cvfit$lambda.min)
coef(cvfitTrain, s="lambda.min")
coef(cvfitTrain, s="lambda.1se")


c = as.matrix(coef(cvfit, s="lambda.1se"))
yHat = predict(fit, xTrain, s=0.0003154)
dof = length(yTrain) - length(c)
rse = sqrt(sum(yTrain - yHat)^2) / dof
rse

yPredict = predict(cvfitTrain, xTest, s=.003154) #0.01810307
dof = length(yTest) - length(c)
rsePredict = sqrt(sum(yTest - yPredict)^2) / dof #0.01781806
rsePredict


cvfit2 <- glmnet::cv.glmnet(datam, fundm,alpha=1,nfolds=10)
cf<-coef(cvfit2, s = "lambda.1se")
i<-which(cvfit$lambda == cvfit$lambda.1se)
e<-cvfit$cvm[i]
r2<-1-e/var(y)
r2 # 0.9507492

r2 <- fit$glmnet.fit$dev.ratio[which(fit$glmnet.fit$lambda == fit$lambda.1se)] 
r2


