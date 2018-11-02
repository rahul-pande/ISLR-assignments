## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      tidy=TRUE,
                      fig.align='center',
                      tidy.opts=list(width.cutoff=60))
# https://www.calvin.edu/~rpruim/courses/s341/S17/from-class/MathinRmd.html

## ----6.8 6 a-------------------------------------------------------------
# Initialize values
y = 1
lambda = 1
beta = seq(-5, 5, .05)

# Plot the ridge optimization vs beta
plot(beta, (y - beta)^2 + lambda * beta^2, xlab = "Beta", ylab = "Ridge")

# Calculate the ridge regression estimation of Beta (equation 6.14)
betaEst = y / (1 + lambda)
ridgeBetaEst = (y - betaEst)^2 + lambda * betaEst^2

# Add the point of BetaEst to the plot
points(betaEst, ridgeBetaEst, col = "green", lwd = 5)

## ----6.8 6 b-------------------------------------------------------------
# Initialize values
y = 1
lambda = 1
beta = seq(-5, 5, .05)

# Plot the lasso optimization vs beta
plot(beta, (y - beta)^2 + lambda * abs(beta), xlab = "Beta", ylab = "Lasso")

# Calculate the lasso regression estimation of Beta (equation 6.15)
betaEst = y  - lambda / 2
ridgeBetaEst = (y - betaEst)^2 + lambda * abs(betaEst)

# Add the point of BetaEst to the plot
points(betaEst, ridgeBetaEst, col = "green", lwd = 5)

## ----6.8 8 a-------------------------------------------------------------
set.seed(123)
x = rnorm(100)
noise = rnorm(100)

## ----6.8 8 b-------------------------------------------------------------
beta0 = 4
beta1 = 0.3
beta2 = 1
beta3 = 3
Y = beta0 * 1 + beta1 * x + beta2 * x^2 + beta3 * x^3 + noise

## ----6.8 8 c-------------------------------------------------------------
library(leaps)
data_ <- data.frame(y = Y, x = x)
reg.fit <- regsubsets(Y ~ poly(x, 10, raw = TRUE), data = data_, nvmax = 100)

summary(reg.fit)
names(summary(reg.fit))

summary.as.data.frame <- function(df, vars){
  return(data.frame(lapply(vars, function(x){df[x]})))
}

reg.fit.summary <- summary(reg.fit)

vars <- c("cp", "bic", "adjr2")
plot.data <- summary.as.data.frame(reg.fit.summary, vars)
plot.data$n <- as.numeric(rownames(plot.data))

lapply(plot.data, function(x){which(x == min(x))})

par(mfrow=c(2,2))
lapply(vars, function(m){
  plot(formula(stringr::str_interp("${m} ~ n")), plot.data)
  })

coefficients(reg.fit, id = 5)
coefficients(reg.fit, id = 2)
coefficients(reg.fit, id = 1)

## ----6.8 8 d 1, message=FALSE--------------------------------------------

## forward selection
reg.fit.forward <- regsubsets(Y ~ poly(x, 10, raw = TRUE), data = data_, nvmax = 100,
                              method = "forward")

reg.fit.forward.summary <- summary(reg.fit.forward)

plot.data <- summary.as.data.frame(reg.fit.forward.summary, vars)
plot.data$n <- as.numeric(rownames(plot.data))
lapply(plot.data, function(x){which(x == min(x))})

par(mfrow=c(2,2))
lapply(vars, function(m){
  plot(formula(stringr::str_interp("${m} ~ n")), plot.data)
  })

coefficients(reg.fit.forward, id = 3)
coefficients(reg.fit.forward, id = 2)
coefficients(reg.fit.forward, id = 1)

## ----6.8 8 d 2, message=FALSE--------------------------------------------

## backward selection
reg.fit.backward <- regsubsets(Y ~ poly(x, 10, raw = TRUE), data = data_, nvmax = 100,
                              method = "backward")

reg.fit.backward.summary <- summary(reg.fit.backward)

plot.data <- summary.as.data.frame(reg.fit.backward.summary, vars)
plot.data$n <- as.numeric(rownames(plot.data))

lapply(plot.data, function(x){which(x == min(x))})

par(mfrow=c(2,2))
lapply(vars, function(m){
  plot(formula(stringr::str_interp("${m} ~ n")), plot.data)
  })

coefficients(reg.fit.backward, id = 5)
coefficients(reg.fit.backward, id = 1)

## ----6.8 8 e-------------------------------------------------------------
library(glmnet)
xmatrix = model.matrix(Y ~ poly(x, 10, raw = T), data = data_)[, -1]
fit.lasso = cv.glmnet(xmatrix, Y, alpha = 1)
best.lambda = fit.lasso$lambda.min
cat(best.lambda)

plot(fit.lasso)


best.fit = glmnet(xmatrix, Y, alpha = 1)
predict(best.fit, s = best.lambda, type = "coefficients")


## ----6.8 8 f 1-----------------------------------------------------------
beta7 = 5
Y = beta0 * 1 + beta7 * x^7 + noise

data_ = data.frame(y = Y, x = x)
reg.fit = regsubsets(y ~ poly(x, 10, raw = T), data = data_, nvmax = 10)

reg.fit.summary <- summary(reg.fit)
plot.data <- summary.as.data.frame(reg.fit.summary, vars)
plot.data$n <- as.numeric(rownames(plot.data))

lapply(plot.data, function(x){which(x == min(x))})

coefficients(reg.fit, id = 1)

## ----6.8 8 f 2-----------------------------------------------------------
# lasso
xmatrix = model.matrix(Y ~ poly(x, 10, raw = T), data = data_)[, -1]
fit.lasso = cv.glmnet(xmatrix, Y, alpha = 1)
best.lambda = fit.lasso$lambda.min
cat(best.lambda)

best.fit = glmnet(xmatrix, Y, alpha = 1)
predict(best.fit, s = best.lambda, type = "coefficients")

## ----6.8 9 a-------------------------------------------------------------
# Load the library and the dataset
library(ISLR)
attach(College)

# Set random seed
set.seed(99)

# Splitting the data according to a 70/30 ratio
split = sample(nrow(College),nrow(College)*0.7)
train = College[split,]
test = College[-split,]


## ----6.8 9 b-------------------------------------------------------------
# Fit a linear model on the training set
fitLm <- lm(Apps ~ ., data = train)

# Predict the values of the testing set using the model
predLm <- predict(fitLm, test)

# Calculate and display the MSE
lmMSE = mean((predLm - test$Apps)^2)
lmMSE


## ----6.8 9 c, message=FALSE----------------------------------------------
 
# Create a matrix for the training and testing sets
trainMat = model.matrix(Apps~.,data=train)
testMat = model.matrix(Apps~.,data=test)

# Define a grid to cover the range of lambda
grid = 10^seq(4,-2,length=100)

# Fit the ridge regression
library(glmnet)
ridge = glmnet(trainMat,train$Apps,alpha=0,lambda=grid,thresh = 1e-12)

# Cross validating the model
crossVRidge = cv.glmnet(trainMat,train$Apps,alpha=0,lambda=grid,thresh=1e-12)

# Find the minimum lambda for which the cross validation error is minimal
estRidge = crossVRidge$lambda.min

# Use this lambda value directly on the testing set to predict
predRidge <-predict(ridge, s=estRidge, newx=testMat)

# Calculate and display the MSE
ridgeMSE = mean((test$Apps-predRidge)^2)
ridgeMSE


## ----6.8 9 d, message=FALSE----------------------------------------------

# Fit the lasso regression
lasso = glmnet(trainMat, train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)

# Cross validating the model
crossVLasso = cv.glmnet(trainMat, train$Apps, alpha = 1, lambda = grid, thresh = 1e-12)

# Find the minimum lambda for which the cross validation error is minimal
estLasso = crossVLasso$lambda.min

# Use this lambda value directly on the testing set to predict
predLasso = predict(lasso, s = estLasso, newx = testMat)

# Calculate and display the MSE
lassoMSE = mean((predLasso - test$Apps)^2)
lassoMSE


## ----6.8 9 e i, message=FALSE--------------------------------------------
 
# Import the library
library(pls)

# Fit a pcr model on the training set
pcrFit = pcr(Apps ~ ., data = train, scale = TRUE, validation = "CV")

# Plot MSEP vs the number of predictors for PCR
validationplot(pcrFit, val.type="MSEP")


## ----6.8 9 e ii, message=FALSE-------------------------------------------
# Choose the number of components based on the previous plot 
predPcr <- predict(pcrFit, test, ncomp = 17)

# Calculate and display the MSE
pcrMSE = mean((predPcr - test$Apps)^2)
pcrMSE


## ----6.8 9 f i, message=FALSE--------------------------------------------
 
# Import the library
library(pls)

# Fit a pcr model on the training set
plsFit = plsr(Apps ~ ., data = train, scale = TRUE, validation = "CV")

# Plot MSEP vs the number of predictors for PLS
validationplot(plsFit, val.type="MSEP")


## ----6.8 9 f ii, message=FALSE-------------------------------------------
# Choose the number of components based on the previous plot 
predPls <- predict(plsFit, test, ncomp = 15)

# Calculate and display the MSE
plsMSE = mean((predPls - test$Apps)^2)
plsMSE


## ----6.8 9 g-------------------------------------------------------------

# Calculate the mean of apps in the testing set
avg = mean(test$Apps)

# Calculate the R2 for the linear model
lmR2 = 1 - mean((predLm - test$Apps)^2) / mean((avg - test$Apps)^2)

# Calculate the R2 for the ridge regression model
ridgeR2 <- 1 - mean((predRidge - test$Apps)^2) / mean((avg - test$Apps)^2)

# Calculate the R2 for the lasso regression model
lassoR2 <- 1 - mean((predLasso - test$Apps)^2) / mean((avg - test$Apps)^2)

# Calculate the R2 for the PCR model
pcrR2 <- 1 - mean((predPcr - test$Apps)^2) / mean((avg - test$Apps)^2)

# Calculate the R2 for the PLS model
plsR2 <- 1 - mean((predPls - test$Apps)^2) / mean((avg - test$Apps)^2)


## ----table2, echo=FALSE, message=FALSE, warnings=FALSE, results='asis'----
tabl <- "
|              |      MSE     |    $R^2$    |
|--------------|-------------:|------------:|
| Linear Model |  `r lmMSE`   | `r lmR2`    |
| Ridge        | `r ridgeMSE` | `r ridgeR2` |
| Lasso        | `r lassoMSE` | `r lassoR2` |
| PCR          | `r pcrMSE`   | `r pcrR2`   |
| PLS          | `r plsMSE`   | `r plsR2`   |
"
cat(tabl)

