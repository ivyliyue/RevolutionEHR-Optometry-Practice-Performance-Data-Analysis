

path <- "/Users/ivyli66/Desktop/optical"
setwd(path)

library(corrplot)
library(ggplot2)
library(glmnet)
library(MASS)
library(Amelia)
library(GGally)
library(car)
library(VIF)
library(psych)
library(pls)
library(pillar)
library(leaps)

eyecapture <- read.csv("LogCountPerformance.csv", header = T)



cor.capture = cor(eyecapture)
cor.capture

head(eyecapture)

captureRate <- eyecapture[1]
x <- eyecapture[,2:25]
head(x)

captureRate <- lasso_TRAIN$eyeglasscaptureratebyordernumerator


#define the X-matrix as a matrix in the data frame
eyecapture$X <- as.matrix(eyecapture[, 2:25])

#first we consider a random selection of 4 properties as a TEST set
lasso$train <- TRUE
lasso$train[sample(1:length(lasso$train), 860)] <- FALSE
lasso_TEST <- lasso[lasso$train == FALSE,]
lasso_TRAIN <- lasso[lasso$train == TRUE,]
head(eyecapture_TRAIN)
mod1 <- pcr(captureRate ~ .
            , ncomp = 4, data= eyecapture_TRAIN, 
            validation = "LOO")
plot(mod1)
plot(mod6, "validation", estimate = c("train", "CV"))

plot(mod6, "validation", estimate = c("train", "CV"), 
     val.type = "R2")

mod7 <- pcr(captureRate ~ .
            , ncomp = 4, data= eyecapture_TRAIN, 
            validation = "LOO")
#scores
scoreplot(mod1, comps = 1:4)
loadingplot(mod1, comps = 1:4, scatter = T)

#choose 3 components
mod3 <- pcr(captureRate ~ .
            , ncomp = 3, data= lasso_TRAIN, 
            validation = "LOO", jackknife = TRUE)

mod4 <- pcr(captureRate ~ .
            , ncomp = 4, data= lasso_TRAIN, 
            validation = "LOO", jackknife = TRUE)
print(mod4$loadings)
mod6 <- pcr(captureRate ~ .
            , ncomp = 6, data= lasso_TRAIN, 
            validation = "LOO", jackknife = TRUE)

print(mod6$loadings, cutoff=.15, sort = T)
plot(mod3)
plot(mod3, "validation", estimate = c("train", "CV"))

plot(mod3, "validation", estimate = c("train", "CV"), 
     val.type = "R2")

plot(mod4, "validation", estimate = c("train", "CV"), 
     val.type = "R2")

compnames(mod3, comps = 1:3, explvar = T)
print(mod6$scores)

#validate with 3 components
k = 3
obsfit <- predplot(mod3, which = "validation")
Residuals <- obsfit[,1] - obsfit[,2]
#plot(obsfit[,2], Residuals, type = "n", main = k,
     xlab = "Fitted", ylab = "Residuals")
qqnorm(Residuals)

corrplot(lasso)
# Plot coefficients with uncertainty from Jacknife:
obsfit <- predplot(mod3, which = "validation")
abline(lm(obsfit[,2] ~ obsfit[,1]))
plot(mod, "validation", estimate = c("train", "CV"), val.type = "R2",
     legendpos = "bottomright")
coefplot(mod3, se.whiskers = TRUE, labels = prednames(mod3), cex.axis = 0.5)
coefplot(mod6, se.whiskers = TRUE, labels = prednames(mod3), cex.axis = 0.5)
biplot(mod3)
jack.test(mod3, ncomp = 3)
jack.test(mod6, ncomp = 6)
summary(mod6)
print(mod3)

rmsep <- sqrt(mean(lasso_TEST$eyeglasscaptureratebyordernumerator - preds)^2)
rmsep
mod3

rmseppreds <- predict(mod3, newdata = lasso_TEST, comps = 3, main="Predict Test Set")
rmseppreds
plot(rmseppreds)
plot(lasso_TEST$eyeglasscaptureratebyordernumerator, preds)
df <- data.frame(mod3$scores[,1], mod3$scores[,2])
colnames(df) <- c('Comp1', 'Comp2')
print(mod3$loadings)

#PCA
PCA_Plot = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = data.frame(pcaData$rotation, .names = row.names(pcaData$rotation))
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

PCA_Plot_Psyc = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  p + geom_text(data=loadings, mapping=aes(x = PC1, y = PC2, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC1", y = "PC2")
}

PCA_Plot_Psyc_Secondary = function(pcaData)
{
  library(ggplot2)
  
  theta = seq(0,2*pi,length.out = 100)
  circle = data.frame(x = cos(theta), y = sin(theta))
  p = ggplot(circle,aes(x,y)) + geom_path()
  
  loadings = as.data.frame(unclass(pcaData$loadings))
  s = rep(0, ncol(loadings))
  for (i in 1:ncol(loadings))
  {
    s[i] = 0
    for (j in 1:nrow(loadings))
      s[i] = s[i] + loadings[j, i]^2
    s[i] = sqrt(s[i])
  }
  
  for (i in 1:ncol(loadings))
    loadings[, i] = loadings[, i] / s[i]
  
  loadings$.names = row.names(loadings)
  
  print(loadings)
  p + geom_text(data=loadings, mapping=aes(x = PC3, y = PC4, label = .names, colour = .names, fontface="bold")) +
    coord_fixed(ratio=1) + labs(x = "PC3", y = "PC4")
}

PCA_capture <- read.csv("LogCountPerformance.csv", header = T)

p = prcomp(PCA_capture, center = T, scale=T)
plot(p)
abline(1, 0, col="red")
summary(p)

rawLoadings=p$rotation %*%diag(p$sdev, nrow(p$rotation), nrow(p$rotation))
print(rawLoadings)
v=varimax(rawLoadings)
ls(v)
v

p2 = psych::principal(PCA_capture, rotate = "varimax", nfactors = 4, scores = T)
p2

print(p2$loadings, cutoff = .4, sort = T)
#print(p2$loadings, cutoff = .5, sort = T)
#print(p2$loadings, cutoff = .7, sort = T)

capture <- PCA_capture$eyeglasscaptureratebyordernumerator


p2$values
p2$rotation
p2$fit
Scores <- as.data.frame(p2$scores)
Scores
RC1 <- Scores$RC1
RC2 <- Scores$RC2
RC2 <- Scores$RC2
RC3 <- Scores$RC3
RC4 <- Scores$RC4

PCA_fit <- lm(capture ~ RC1 + RC2 +RC3 +RC4, data = PCA_capture)
PCA_fit
summary(PCA_capture)

plot(PCA_fit)
