## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      tidy=TRUE,
                      fig.align='center',
                      tidy.opts=list(width.cutoff=60))
# https://www.calvin.edu/~rpruim/courses/s341/S17/from-class/MathinRmd.html

## ----6 11 a i, echo=TRUE-------------------------------------------------
library(MASS)
library(leaps)
library(glmnet)
attach(Boston)

colSums(sapply(Boston, is.na))
# No NAs in the dataset

# k-fold cross validation
k = 10

n = dim(Boston)[1]
p = dim(Boston)[2]-1

set.seed(123)
folds = sample(rep(1:k, length = nrow(Boston)), replace = T)

form.subset = as.formula("crim ~ .")

cv.errors = matrix(NA, k, p)

for (i in 1:k) {
  subset.fit <- regsubsets(form.subset, Boston[folds!=i, ], nvmax = p)
  for (n.subset in 1:p) {
    m.mat <- model.matrix(form.subset, Boston[folds==i, ])
    best.coef <- coef(subset.fit, id=n.subset)
    pred <- m.mat[, names(best.coef)] %*% best.coef
    # mean squared error
    error = mean((Boston[folds==i, ]$crim - pred)^2)
    cv.errors[i,n.subset] = error
  }
  
}
# root mean squared values
rmse.cv = sqrt(apply(cv.errors, 2, mean))
plot(rmse.cv, type = "b")

which(rmse.cv == min(rmse.cv))
rmse.cv[which(rmse.cv == min(rmse.cv))]

## ----6 11 a ii, echo=TRUE------------------------------------------------
# Lasso regression

X = model.matrix(crim ~ ., data = Boston)[, -1]
y = Boston$crim
cv.lasso = cv.glmnet(X, y, type.measure = "mse", nfolds = 10)
plot(cv.lasso)

coef(cv.lasso)[,1]

# One standard error lamda to avoid overfitting
chosen.lambda = cv.lasso$lambda.1se

# root mean square error for the chosen lamdba
sqrt(cv.lasso$cvm[cv.lasso$lambda == chosen.lambda])


## ----6 11 a iii, echo=TRUE-----------------------------------------------

cv.ridge = cv.glmnet(X, y, type.measure = "mse", alpha = 0, nfolds = 10)
plot(cv.ridge)

coef(cv.ridge)

# One standard error lamda to avoid overfitting
chosen.lambda = cv.ridge$lambda.1se

# root mean square error for the chosen lamdba
sqrt(cv.ridge$cvm[cv.ridge$lambda == chosen.lambda])


## ----6 11 a iv, echo=TRUE------------------------------------------------
library(pls)

pcr.fit = pcr(crim ~ ., data = Boston, scale = TRUE, validation = "CV", segments= 10)
summary(pcr.fit)

# variance explanation
plot(pcr.fit$Xvar/sum(pcr.fit$Xvar))

# validation plot
validationplot(pcr.fit,val.type='MSEP')

ncomp = 4

set.seed(123)
folds = sample(rep(1:k, length = nrow(Boston)), replace = T)

cv.errors = rep(0, k)

for (i in 1:k) {
  pcr.fit <- pcr(crim ~ ., data = Boston[folds !=i, ], scale = TRUE)
  pred = predict( pcr.fit, Boston[folds==i, ], ncomp=ncomp)
  error = mean((Boston[folds==i, ]$crim - pred)^2)
  cv.errors[i] = error
}
sqrt(mean(cv.errors))


## ----6 11 b, echo=TRUE---------------------------------------------------
results <- rbind(
  c("Best Subset", 6.633116, 9),
  c("Lasso Regression", 7.549995, 1),
  c("Ridge Regression", 7.359946, 13),
  c("PCR", 6.875975, 4)
)
colnames(results) <- c("Method", "MSE", "# predictors")
knitr::kable(results)

## ----6.8 2, echo=TRUE----------------------------------------------------
x_lower = seq(-2, 1, by = 0.05)
x_upper = seq(1, 2, by = 0.05)

y_lower = 1 + x_lower
y_upper = -1 + 5 * x_upper - 2 * (x_upper ^ 2)

x <- c(x_lower, x_upper)
y <- c(y_lower, y_upper)

plot(x,y)

## ----7 4, echo=TRUE------------------------------------------------------
x = seq(-2, 2, 0.05)
y = 1 + 1 * I(x <= 2 & x >= 0) - (x-1) * I(x <= 2 & x >= 1)  + 3 * (x-3) * I (x <= 4 & x >= 3) + I(x <= 5 & x>4)

plot(x, y)

# y-intercept
y[which(x==0)]

## ----7.9 6 a i, message=FALSE--------------------------------------------
# Import the required libraries
library(ISLR)
library(boot)

# Fix the random seed
set.seed(4)

# Initialize the error/degree vector
errors = rep(NA, 10)

# For each degree from 1 to 10
for (d in 1:10) {
    # Fit a polynomial model of degree d
    fit = glm(wage ~ poly(age, d), data = Wage)
    # Estimate of the test MSE with 10-fold cross validation
    errors[d] = cv.glm(Wage, fit, K = 10)$delta[1]
}
# Plot MSE vs. Degree
plot(1:10, errors, xlab = "Polynomial degree", ylab = "Test MSE", type = "l")

# Highlight the lowest test MSE value in the plot 
points(which.min(errors), errors[which.min(errors)], col = "green", cex = 2.5, pch = 20)

## ----7.9 6 a ii, message=FALSE-------------------------------------------
# We fit different polynomial models going from degree 1 to 10
fit1 = lm(wage ~ age, data = Wage)
fit2 = lm(wage ~ poly(age, 2), data = Wage)
fit3 = lm(wage ~ poly(age, 3), data = Wage)
fit4 = lm(wage ~ poly(age, 4), data = Wage)
fit5 = lm(wage ~ poly(age, 5), data = Wage)
fit6 = lm(wage ~ poly(age, 6), data = Wage)
fit7 = lm(wage ~ poly(age, 7), data = Wage)
fit8 = lm(wage ~ poly(age, 8), data = Wage)
fit9 = lm(wage ~ poly(age, 9), data = Wage)
fit10 = lm(wage ~ poly(age, 10), data = Wage)

# We use the null hypothesis test ANOVA
anova(fit1, fit2, fit3, fit4, fit5, fit6, fit7, fit8, fit9, fit10)

## ----7.9 6 a iii, message=FALSE------------------------------------------
# Plot the Wage vs. Age data
plot(wage ~ age, data = Wage)

# Calculate the age grid (lower and upper limits)
limits = range(Wage$age)
ageGrid = seq(from = limits[1], to = limits[2])

# Fit the data with the quartic polynomial model
fit = lm(wage ~ poly(age, 4), data = Wage)

# Calculate the predictions for this model
predictions = predict(fit, newdata = list(age = ageGrid))

# Display the model on the plot
lines(ageGrid, predictions, col = "red", lwd = 3)

## ----7.9 6 b i, message=FALSE--------------------------------------------
errors <- rep(NA, 10)
for (intervals in 2:10) {
    Wage$age.cut = cut(Wage$age, intervals)
    fit = glm(wage ~ age.cut, data = Wage)
    errors[intervals] = cv.glm(Wage, fit, K = 10)$delta[1]
}
plot(2:10, errors[-1], xlab = "Cuts", ylab = "Test MSE", type = "l")

points(which.min(errors), errors[which.min(errors)], col = "green", cex = 2.5, pch = 20)

## ----7.9 6 b ii----------------------------------------------------------
plot(wage ~ age, data = Wage)
agelims = range(Wage$age)
age.grid = seq(from = agelims[1], to = agelims[2])
fit = glm(wage ~ cut(age, 8), data = Wage)
preds = predict(fit, data.frame(age = age.grid))
lines(age.grid, preds, col = "red", lwd = 3)

## ----7.9 7 i-------------------------------------------------------------
# Fix the random seed
set.seed(1)

 # Display the summary of the features maritl and jobclass (index of values)
summary(Wage[, c("maritl", "jobclass")] )

# Display the wage vs. maril and wage vs. jobclass side by side
par(mfrow = c(1, 2))
plot(Wage$maritl, Wage$wage)
plot(Wage$jobclass, Wage$wage)

## ----7.9 7 ii------------------------------------------------------------
library(gam)
fit0 = gam(wage ~ lo(age,span=200,degree=1), data = Wage)
fit1 = gam(wage ~ lo(age,span=200,degree=1) + year, data = Wage)
fit2 = gam(wage ~ s(year,4)+lo(age,span=200,degree=1), data = Wage)
fit4 = gam(wage ~ s(year,4)+lo(age,span=200,degree=1) + education, data = Wage)
fit5 = gam(wage ~ s(year,4)+lo(age,span=200,degree=1) + education + jobclass, data = Wage)
fit6 = gam(wage ~ s(year,4)+lo(age,span=200,degree=1) + education + maritl, data = Wage)
fit7 = gam(wage ~ s(year,4)+lo(age,span=200,degree=1) + education + jobclass + maritl, data = Wage)
anova(fit0, fit1, fit2, fit4, fit5, fit6, fit7)

## ----7.9 7 iii-----------------------------------------------------------
attach(Wage)

# Calculate the prediction with our model
myPreds = predict(fit6,se=TRUE)

# Plot the data
plot(age,wage, col='red')

# Plot the predictions
points(age,myPreds$fit,col='green',pch=20)

# Fancy GAM plot
par(mfrow = c(2, 2), pty = "s")
plot(fit6,se=TRUE)


## ----8.4 setup, echo=FALSE-----------------------------------------------
partition_plot <- function (tree, label = "yval", add = FALSE, ordvars, ...) 
{
    ptXlines <- function(x, v, xrange, xcoord = NULL, ycoord = NULL, 
        tvar, i = 1L) {
        if (v[i] == "<leaf>") {
            y1 <- (xrange[1L] + xrange[3L])/2
            y2 <- (xrange[2L] + xrange[4L])/2
            return(list(xcoord = xcoord, ycoord = c(ycoord, y1, 
                y2), i = i))
        }
        if (v[i] == tvar[1L]) {
            xcoord <- c(xcoord, x[i], xrange[2L], x[i], xrange[4L])
            xr <- xrange
            xr[3L] <- x[i]
            ll2 <- Recall(x, v, xr, xcoord, ycoord, tvar, i + 
                1L)
            xr <- xrange
            xr[1L] <- x[i]
            return(Recall(x, v, xr, ll2$xcoord, ll2$ycoord, tvar, 
                ll2$i + 1L))
        }
        else if (v[i] == tvar[2L]) {
            xcoord <- c(xcoord, xrange[1L], x[i], xrange[3L], 
                x[i])
            xr <- xrange
            xr[4L] <- x[i]
            ll2 <- Recall(x, v, xr, xcoord, ycoord, tvar, i + 
                1L)
            xr <- xrange
            xr[2L] <- x[i]
            return(Recall(x, v, xr, ll2$xcoord, ll2$ycoord, tvar, 
                ll2$i + 1L))
        }
        else stop("wrong variable numbers in tree.")
    }
    if (inherits(tree, "singlenode")) 
        stop("cannot plot singlenode tree")
    if (!inherits(tree, "tree")) 
        stop("not legitimate tree")
    frame <- tree$frame
    leaves <- frame$var == "<leaf>"
    var <- unique(as.character(frame$var[!leaves]))
    if (length(var) > 2L || length(var) < 1L) 
        stop("tree can only have one or two predictors")
    nlevels <- sapply(attr(tree, "xlevels"), length)
    if (any(nlevels[var] > 0L)) 
        stop("tree can only have continuous predictors")
    x <- rep(NA, length(leaves))
    x[!leaves] <- as.double(substring(frame$splits[!leaves, "cutleft"], 
        2L, 100L))
    m <- model.frame(tree)
    if (length(var) == 1L) {
        x <- sort(c(range(m[[var]]), x[!leaves]))
        if (is.null(attr(tree, "ylevels"))) 
            y <- frame$yval[leaves]
        else y <- frame$yprob[, 1L]
        y <- c(y, y[length(y)])
        if (add) 
            lines(x, y, type = "s", ...)
        else {
            a <- attributes(attr(m, "terms"))
            yvar <- as.character(a$variables[1 + a$response])
            xo <- m[[yvar]]
            if (is.factor(xo)) 
                ylim <- c(0, 1)
            else ylim <- range(xo)
            plot(x, y, ylab = yvar, xlab = var, type = "s", ylim = ylim, 
                xaxs = "i", ...)
        }
        invisible(list(x = x, y = y))
    }
    else {
        if (!missing(ordvars)) {
            ind <- match(var, ordvars)
            if (any(is.na(ind))) 
                stop("unmatched names in vars")
            var <- ordvars[sort(ind)]
        }
        lab <- frame$yval[leaves]
        if (is.null(frame$yprob)) 
            lab <- format(signif(lab, 3L))
        else if (match(label, attr(tree, "ylevels"), nomatch = 0L)) 
            lab <- format(signif(frame$yprob[leaves, label], 3L))
        rx <- range(m[[var[1L]]])
        rx <- rx + c(-0.025, 0.025) * diff(rx)
        rz <- range(m[[var[2L]]])
        rz <- rz + c(-0.025, 0.025) * diff(rz)
        xrange <- c(rx, rz)[c(1, 3, 2, 4)]
        xcoord <- NULL
        ycoord <- NULL
        xy <- ptXlines(x, frame$var, xrange, xcoord, ycoord, 
            var)
        xx <- matrix(xy$xcoord, nrow = 4L)
        yy <- matrix(xy$ycoord, nrow = 2L)
        if (!add) 
            plot(rx, rz, xlab = var[1L], ylab = var[2L], type = "n", 
                xaxs = "i", yaxs = "i", ...)
        segments(xx[1L, ], xx[2L, ], xx[3L, ], xx[4L, ])
        text(yy[1L, ], yy[2L, ], as.character(paste("R", 1:length(lab), sep = "")), ...)
    }
}

## ----8.4 1---------------------------------------------------------------
library(tree)
syn_data <- data.frame(mvrnorm(n= 100, mu = c(10,15), matrix(c(10,3,3,5),2,2)))
color <- sample(c("red", "blue"), nrow(syn_data), replace = T)
syn_data$class <- ifelse(color == "red", 1, 0)
tree.fit <- tree(class ~ ., data = syn_data, control = tree.control(nobs = nrow(syn_data), mindev = 0.02, minsize = 30))

partition_plot(tree.fit)

## ----8.4 8 a-------------------------------------------------------------

library(ISLR)
set.seed(1)

# Splitting the data according to a 50/50 ratio

split = sample(1:nrow(Carseats), nrow(Carseats) / 2)
Carseats.train = Carseats[split, ]
Carseats.test = Carseats[-split, ]


## ----8.4 8 b-------------------------------------------------------------
# Import the library
library(tree)

tree.carseats =  tree(Sales ~ ., data = Carseats.train)
summary(tree.carseats)
plot(tree.carseats)
text(tree.carseats, pretty = 0)


## ----8.4 8 b ii----------------------------------------------------------

pred <- predict(tree.carseats, newdata = Carseats.test)
mean((pred - Carseats.test$Sales)^2)


## ----8.4 8 c i-----------------------------------------------------------
cv.carseats <- cv.tree(tree.carseats)
plot(cv.carseats$size, cv.carseats$dev, type = "b")
tree.min <- which.min(cv.carseats$dev)
points(tree.min, cv.carseats$dev[tree.min], col = "red", cex = 2, pch = 20)


## ----8.4 8 c ii----------------------------------------------------------

prune.carseats = prune.tree(tree.carseats, best = 8)
plot(prune.carseats)
text(prune.carseats, pretty = 0)
pred = predict(prune.carseats, newdata = Carseats.test)
mean((pred - Carseats.test$Sales)^2)

## ----8.4 8 d i-----------------------------------------------------------
library(randomForest)

bag.carseats <- randomForest(Sales ~ ., data = Carseats.train, mtry = 10, ntree = 500, importance = TRUE)
pred.bag <- predict(bag.carseats, newdata = Carseats.test)
mean((pred.bag - Carseats.test$Sales)^2)

## ----8.4 8 d ii----------------------------------------------------------

importance(bag.carseats)


## ----8.4 8 e i-----------------------------------------------------------
mse.vec <- NA
for (a in 1:10){
  rf.carseats <-  randomForest(Sales ~ . , data=Carseats.train, 
                             mtry=a, ntree=500, importance=TRUE)
  rf.pred <-  predict(rf.carseats, Carseats.test)
  mse.vec[a] <- mean((Carseats.test$Sales - rf.pred)^2)
}

# Number of variables used in the best model
which.min(mse.vec)

# Test MSE corresponding to the best model
mse.vec[which.min(mse.vec)]


## ----8.4 8 e ii----------------------------------------------------------

# Most important variables corresponding to the best model
rf.carseats <-  randomForest(Sales ~ . , data = Carseats, 
                             mtry=9, ntree=500, importance=TRUE)
importance(rf.carseats)


