## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE, tidy.opts=list(width.cutoff=60), tidy=TRUE)
# https://www.calvin.edu/~rpruim/courses/s341/S17/from-class/MathinRmd.html

## ----2.4 a, echo=FALSE---------------------------------------------------
curve(150*x, from=1, to=9, xlab="flexibility", ylab="", col="white")

# (1) bias
curve(2000 * exp(-(x*0.8)) +100, add=TRUE, col="cyan2", lwd=2)
# (2) variance
curve(exp(x/1.5)+150, add=TRUE, col="orange", lwd=2)
# (3) irreducible error
curve(0*x+250, add=TRUE, col="gray", lwd=2, lty=2)                  

# training error (1+3)
curve(2000 * exp(-(x*0.8)) +100+0*x+250, add=TRUE, col="blue", lwd=2)
# MSE test error (1+2)
curve(2000 * exp(-(x*0.8)) +100+exp(x/1.5)+150, add=TRUE, col="red", lwd=2)

legend(10, legend=c("Bias", "Variance", "Train Error",
                    "Test Error", "Irreducible Error"),
       col=c("blue", "orange", "blue", "red", "gray"), lty=1:2, cex=0.3)

## ----2.4 8a, echo=TRUE---------------------------------------------------
college <- read.csv("College.csv", stringsAsFactors = TRUE)

## ----2.4 8b, echo=TRUE---------------------------------------------------
# fix(college)
rownames(college) = college[,1]
college = college[,-1]
# fix(college)

## ----2.4 8c i, echo=TRUE-------------------------------------------------
summary(college)

## ----2.4 8c ii, echo=TRUE------------------------------------------------
pairs(college[,1:10])

## ----2.4 8c iii, echo=TRUE, fig.align='center'---------------------------
plot(Outstate~Private, data=college)

## ----2.4 8c iv, echo=TRUE, fig.align='center'----------------------------
Elite = rep("No",nrow(college))
Elite[college$Top10perc > 50] = " Yes"
Elite = as.factor(Elite)
college = data.frame(college, Elite)
summary(college)
plot(Outstate~Elite, data=college)

## ----2.4 8c v, echo=TRUE, results='hide', fig.height= 8, fig.align='center'----
par(mfrow=c(2,2))

hist_vars   = c("Apps", "S.F.Ratio", "Room.Board", "Grad.Rate")
hist_breaks = c(50, 10, 20, 20)
hist_data = subset(college, select = hist_vars)
make_hist <- function(list.elem, names, breaks) {
  hist(list.elem, main = names, xlab = names, breaks = breaks)
}
mapply(make_hist, 
       list.elem = hist_data, 
       names = names(hist_data),
       breaks = hist_breaks)

## ----2.4 8c vi, echo=TRUE, results='hide', fig.height= 8, fig.align='center'----
college$Accept.Rate = college$Accept / college$Apps * 100
college$Enroll.Rate = college$Enroll / college$Accept * 100

par(mfrow=c(2,2))
plot(Grad.Rate ~ Outstate, data = college)
plot(Outstate ~ Private, data = college)
plot(Outstate ~ S.F.Ratio, data = college)
plot(S.F.Ratio ~ Private, data = college)

par(mfrow=c(2,2))
plot(Apps ~ Private, data = college)
plot(Enroll.Rate ~ Outstate, data = college)

## ----2.4 9 setup, echo=TRUE----------------------------------------------
library(ISLR)
data(Auto)
colSums(sapply(Auto, is.na))

## ----2.4 9a, echo=TRUE, fig.align='center'-------------------------------
summary(Auto)
hist(Auto$origin, xlab = "origin", main = "Histogram of origin")

## ----2.4 9b, echo=TRUE, fig.align='center', tidy=TRUE--------------------
quantitative_preds = c("mpg", "cylinders", "displacement", "horsepower", "weight",
                       "acceleration", "year")
qualitative_preds = c("origin", "name")

ranges = data.frame(lapply(subset(Auto, select = quantitative_preds), range))
knitr::kable(ranges)

## ----2.4 9c, echo=TRUE, fig.align='center'-------------------------------
mean_sd = data.frame(lapply(subset(Auto, select = quantitative_preds), function(x) {
  c(mean = mean(x), std_dev = sd(x))
  }
))
knitr::kable(mean_sd)

## ----2.4 9d, echo=TRUE, fig.align='center'-------------------------------
subset_mean_sd = data.frame(lapply(subset(Auto[- c(10:85),], select = quantitative_preds),
                                   function(x) {
                                     c(range = range(x), mean = mean(x), std_dev = sd(x))
                                     }
                                   )
                            )
knitr::kable(subset_mean_sd)

## ----2.4 9e, echo=TRUE, fig.height=10, fig.width=10, fig.align='center'----
factor_vars = c("origin")
newAuto = Auto
newAuto[factor_vars] = lapply(Auto[factor_vars], as.factor)
pairs(newAuto)

## ----2.4 9f, echo=TRUE, fig.height=3-------------------------------------
par(mfrow = c(1,3))
plot(mpg ~ cylinders + displacement + horsepower + weight + acceleration +
       year + origin, data = newAuto)

fit = lm(mpg ~ cylinders + displacement + horsepower + weight + acceleration +
           year + origin, data = newAuto)
summary(fit)

## ----3.7 8a, echo=TRUE---------------------------------------------------

# Import the ISLR package
library(ISLR)

# Attach the Auto as the current database
attach(Auto)

# Perform a simple linear regression
lmFit=lm(mpg~horsepower)

#Print the results
summary(lmFit)

# Calculate the mean of the response (mpg) (question ii.)
mean(mpg)

## ----3.7 8a iv-----------------------------------------------------------
predict(lmFit, data.frame(horsepower=98), interval="confidence")
predict(lmFit, data.frame(horsepower=98), interval="prediction")

## ----3.7 8b--------------------------------------------------------------
# Plot the response and the predictor
plot(horsepower, mpg)

# Add the least squares regression line
abline(lmFit)

## ----3.7 8c--------------------------------------------------------------
plot(lmFit)

## ----3.7 9a--------------------------------------------------------------
# Produce a scatterplot
pairs(Auto)

## ----3.7 9b--------------------------------------------------------------
# Remove the names variable
names(Auto)

# Compute the correlation matrix
cor(Auto[1:8])

## ----3.7 9c--------------------------------------------------------------
lmFit2 = lm(mpg ~ . - name, data = Auto)
summary(lmFit2)

## ----3.7 9d--------------------------------------------------------------
# 2 x 2 pictures on one plot
par(mfrow = c(2, 2))
# 
plot(lmFit2)

## ----3.7 9e i------------------------------------------------------------
lmFit2 = lm(mpg ~ year + weight + origin + year:origin, data = Auto)
summary(lmFit2)

## ----3.7 9e ii-----------------------------------------------------------
lmFit2 = lm(mpg ~.-name+horsepower:weight, data = Auto)
summary(lmFit2)

## ----3.7 9f i------------------------------------------------------------
lmFit2 = lm(mpg ~.-name+ log(horsepower), data = Auto)
summary(lmFit2)

## ----3.7 9f ii-----------------------------------------------------------
lmFit2 = lm(mpg ~.-name+ I((weight)^2), data = Auto)
summary(lmFit2)

## ----3.7 9f iii----------------------------------------------------------
lmFit2 = lm(mpg ~.-name+ log(horsepower) + I((horsepower)^2), data = Auto)
summary(lmFit2)

## ----3.7 9f iv-----------------------------------------------------------
lmFit2 = lm(mpg ~.-name+ log(displacement) + I((cylinders)^2), data = Auto)
summary(lmFit2)

