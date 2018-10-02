## ----setup, include=FALSE------------------------------------------------
knitr::opts_chunk$set(echo = TRUE,
                      tidy=TRUE,
                      fig.align='center',
                      tidy.opts=list(width.cutoff=60))
# https://www.calvin.edu/~rpruim/courses/s341/S17/from-class/MathinRmd.html

## ----4.7 10 a, echo=TRUE, fig.height= 8, fig.align='center', results='hide'----
require(ISLR)
data("Weekly")
data.weekly = Weekly
summary(data.weekly)

# standardize data
data.weekly[ , colnames(data.weekly) != "Direction"] = scale(
  data.weekly[ , colnames(data.weekly) != "Direction"])

# pair plot
pairs(data.weekly)

# box plots w.r.t. response variables
par(mfrow=c(3,3))

vars = setdiff(names(Weekly), c("Today", "Direction"))

plot_against_direction <- function(y){
  x = "Direction"
  f <- as.formula(paste(c(y, x), collapse = " ~ "))
  boxplot(f, data= Weekly, ylab = y, xlab = x)
}

sapply(vars, plot_against_direction)

## ----4.7 10 b, echo=TRUE-------------------------------------------------
logistic.fit <- glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5 + Volume,
                    data = data.weekly,
                    family = "binomial")
summary(logistic.fit)

## ----4.7 10 c, echo=TRUE-------------------------------------------------
pred <- round(predict(logistic.fit, data.weekly, type = "response"))
pred <- factor(x = pred, labels = c("Down", "Up"))
actual <- data.weekly$Direction
conf_matrix <- table(actual, pred)
message("Confusion matrix")
conf_matrix
message("Fraction of correct predictions:")
(conf_matrix[1] + conf_matrix[4])/sum(conf_matrix)

## ----4.7 10 d, echo=TRUE-------------------------------------------------
train.data <- data.weekly[Weekly$Year <= 2008, ]
heldout.data <- data.weekly[Weekly$Year > 2008, ]

lag2.fit <- glm(Direction ~ Lag2, data = train.data, family = "binomial")

lag2_pred <- round(predict(lag2.fit, heldout.data, type = "response"))
lag2_pred <- factor(x = lag2_pred, labels = c("Down", "Up"))
lag2_actual <- heldout.data$Direction
lag2_conf_matrix <- table(lag2_actual, lag2_pred)
message("Confusion matrix for Lag2 model")
lag2_conf_matrix
message("Fraction of correct predictions:")
(lag2_conf_matrix[1] + lag2_conf_matrix[4])/sum(lag2_conf_matrix)


## ----4.7 10 e, echo=TRUE-------------------------------------------------
require(MASS)
lda.fit <- lda(Direction ~ Lag2, data = train.data)

lda_pred <- predict(lda.fit, heldout.data)$class
lda_actual <- heldout.data$Direction
lda_conf_matrix <- table(lda_actual, lda_pred)
message("Confusion matrix for lda model")
lda_conf_matrix
message("Fraction of correct predictions:")
(lda_conf_matrix[1] + lda_conf_matrix[4])/sum(lda_conf_matrix)

## ----4.7 10 f, echo=TRUE-------------------------------------------------

qda.fit <- qda(Direction ~ Lag2, data = train.data)

qda_pred <- predict(qda.fit, heldout.data)$class
qda_actual <- heldout.data$Direction
qda_conf_matrix <- table(qda_actual, qda_pred)
message("Confusion matrix for lda model")
qda_conf_matrix
message("Fraction of correct predictions:")
(qda_conf_matrix[1] + qda_conf_matrix[4])/sum(qda_conf_matrix)


## ----4.7 10 g, echo=TRUE-------------------------------------------------
require(class)

set.seed(123)

trainX <- as.matrix(train.data$Lag2)
testX <- as.matrix(heldout.data$Lag2)
knn_pred <- knn(trainX, testX, train.data$Direction, k=1)
knn_actual <- heldout.data$Direction
knn_conf_matrix <- table(knn_actual, knn_pred)
message("Confusion matrix for knn=1 model")
knn_conf_matrix
message("Fraction of correct predictions:")
(knn_conf_matrix[1] + knn_conf_matrix[4])/sum(knn_conf_matrix)

## ----4.7 11 a, echo=TRUE, fig.height= 8, fig.align='center', results='hide'----
data("Auto")
Auto$mpg01 <- ifelse(Auto$mpg > median(Auto$mpg), 1, 0)
Auto = Auto[, names(Auto) != "mpg"]
Auto$mpg01 <- as.factor(Auto$mpg01)


## ----4.7 11 b, echo=TRUE, fig.height= 8, fig.align='center', results='hide'----
pairs(Auto)

par(mfrow=c(3,3))

vars = setdiff(names(Auto), c("mpg01", "name"))

plot_against_mpg01 <- function(y){
  x = "mpg01"
  f <- as.formula(paste(c(y, x), collapse = " ~ "))
  boxplot(f, data = Auto, ylab = y, xlab = x)
}

sapply(vars, plot_against_mpg01)

hist(Auto$cylinders[Auto$mpg01 == 0 ], xlab = "cylinders (mpg == 0)", main = "")
hist(Auto$cylinders[Auto$mpg01 == 1 ], xlab = "cylinders (mpg == 1)", main = "")

## ----4.7 11 c, echo=TRUE-------------------------------------------------
perc_train <- 0.8
n_rows <- nrow(Auto)
n_train <- round(perc_train * n_rows)

samples = sample(1:n_rows, n_train, replace=FALSE)
train = logical(n_rows)
train[samples] = TRUE
test = !train

train.data <- Auto[train, ]
test.data <- Auto[test, ]

dim(train.data)
dim(test.data)

## ----4.7 11 d, echo=TRUE-------------------------------------------------
lda.fit <- lda(mpg01 ~ . -name -year -acceleration, data = train.data)

lda_pred <- predict(lda.fit, test.data)$class
lda_actual <- test.data$mpg01
lda_conf_matrix <- table(lda_actual, lda_pred)
message("Confusion matrix for lda model")
lda_conf_matrix
message("Error rate of LDA:")
(lda_conf_matrix[2] + lda_conf_matrix[3])/sum(lda_conf_matrix)

## ----4.7 11 e, echo=TRUE-------------------------------------------------
qda.fit <- qda(mpg01 ~ . -name -year -acceleration, data = train.data)

qda_pred <- predict(qda.fit, test.data)$class
qda_actual <- test.data$mpg01
qda_conf_matrix <- table(qda_actual, qda_pred)
message("Confusion matrix for lda model")
qda_conf_matrix
message("Error rate of QDA:")
(qda_conf_matrix[2] + qda_conf_matrix[3])/sum(qda_conf_matrix)

## ----4.7 11 f, echo=TRUE-------------------------------------------------
consider.columns = !(names(train.data) %in% c("name", "year", "acceleration"))
logistic.fit <- glm(mpg01 ~ ., data = train.data[, consider.columns], family = "binomial")
summary(logistic.fit)

pred <- round(predict(logistic.fit, test.data[, consider.columns], type = "response"))
pred <- factor(x = pred, labels = c("0", "1"))
actual <- test.data$mpg01
conf_matrix <- table(actual, pred)
message("Confusion matrix")
conf_matrix
message("Error rate for logistic regression:")
(conf_matrix[2] + conf_matrix[3])/sum(conf_matrix)

## ----4.7 11 g, echo=TRUE-------------------------------------------------
knn_features <- !(names(train.data) %in% c("name", "year", "acceleration", "mpg01"))
trainX <- as.matrix(train.data[, knn_features])
testX <- as.matrix(test.data[, knn_features])

calculate_knn<- function(i) {
  set.seed(123)
  knn_pred <- knn(trainX, testX, train.data$mpg01, k=i)
  knn_actual <- test.data$mpg01
  knn_conf_matrix <- table(knn_actual, knn_pred)
  # sprintf("Confusion matrix for knn=1 model")
  # print(knn_conf_matrix)
  error_rate <- (knn_conf_matrix[2] + knn_conf_matrix[3])/sum(knn_conf_matrix)
  return(c(i, error_rate))
}

k_data <- data.frame(t(sapply(seq(from=1, to=51, by=2), calculate_knn)))
colnames(k_data) <- c("k", "error_rate")
plot(error_rate ~ k, data=k_data)

best_k <- k_data$k[k_data$error_rate == min(k_data$error_rate)]
best_k <- paste(as.vector(best_k), collapse = ", ")

sprintf("From the plot, k=%s seems to perform better on this dataset", best_k)

## ----5.4 2 g-------------------------------------------------------------
# Assign to x the integer sequence of range (1,100000)
x=1:100000

# Apply the function of probabilty to the previous sequence
y=sapply(x,function(n){1-((1-(1/n))^n)})

# Display the curve
plot(x,y,xlab="n",ylab="Pd",log="x")

## ----5.4 2 h-------------------------------------------------------------
#set.seed(211)
store=rep(NA, 10000)
for(i in 1:10000){
  store[i]=sum(sample(1:100, rep=TRUE)==4)>0 
}
mean(store)

## ----5.4 5 a-------------------------------------------------------------
# Import the library ISLR
library(ISLR)
# Attach Default as the default dataset
attach(Default)
# Fix the random seed
set.seed(5)
# Build a logistic regression model
glmFit = glm(default ~ income + balance, data = Default, family = "binomial")
# Print the model details
summary(glmFit)


## ----5.4 5 b i-----------------------------------------------------------
# Get the sample indices
indices = sample(1:nrow(Default),nrow(Default)*0.8)
# Assign the training subset to a variable
trainingSet = Default[indices, ]
# Assign the testing subset to a variable
testingSet = Default[-indices,]

## ----5.4 5 b ii----------------------------------------------------------
# Train a logistic regression model using the training set only
trainFit = glm(default ~ income + balance, data = trainingSet, family = "binomial")
# Display the model details
summary(trainFit)

## ----5.4 5 b iii---------------------------------------------------------
# Predict the default values of the testing set using the built model (with training set)
prediction = predict(trainFit,testingSet,type="response")

# Classify the default category of the default value
class = ifelse(prediction > 0.5,"Yes","No")

# Display the confusion matrix (in a table)
table(testingSet$default, class, dnn=c("Actual","Predicted"))

## ----5.4 5 b iv----------------------------------------------------------
# Compute the validation set error
mean(class!=testingSet$default)

## ----5.4 5 c i-----------------------------------------------------------
# First sample (70%/30%)
indices = sample(1:nrow(Default),nrow(Default)*0.7)
trainingSet = Default[indices, ]
testingSet = Default[-indices,]
trainFit = glm(default ~ income + balance, data = trainingSet, family = "binomial")
prediction = predict(trainFit,testingSet,type="response")
class = ifelse(prediction > 0.5,"Yes","No")
table(testingSet$default, class, dnn=c("Actual","Predicted"))
mean(class!=testingSet$default)

## ----5.4 5 c ii----------------------------------------------------------
# Second sample (50%/50%)
indices = sample(1:nrow(Default),nrow(Default)*0.5)
trainingSet = Default[indices, ]
testingSet = Default[-indices,]
trainFit = glm(default ~ income + balance, data = trainingSet, family = "binomial")
prediction = predict(trainFit,testingSet,type="response")
class = ifelse(prediction > 0.5,"Yes","No")
table(testingSet$default, class, dnn=c("Actual","Predicted"))
mean(class!=testingSet$default)

## ----5.4 5 c iii---------------------------------------------------------
# Third sample (10%/90%)
indices = sample(1:nrow(Default),nrow(Default)*0.1)
trainingSet = Default[indices, ]
testingSet = Default[-indices,]
trainFit = glm(default ~ income + balance, data = trainingSet, family = "binomial")
prediction = predict(trainFit,testingSet,type="response")
class = ifelse(prediction > 0.5,"Yes","No")
table(testingSet$default, class, dnn=c("Actual","Predicted"))
mean(class!=testingSet$default)

## ----5.4 5 d-------------------------------------------------------------
# We add the dummy variable student to the logistic regression model
indices = sample(1:nrow(Default),nrow(Default)*0.8)
trainingSet = Default[indices, ]
testingSet = Default[-indices,]
trainFit = glm(default ~ income + balance + student, data = trainingSet, family = "binomial")
prediction = predict(trainFit,testingSet,type="response")
class = ifelse(prediction > 0.5,"Yes","No")
table(testingSet$default, class, dnn=c("Actual","Predicted"))
mean(class!=testingSet$default)

## ----5.4 6 a-------------------------------------------------------------
# Fix the random seed
set.seed(5)
# Create a logistic regression model based on income and balance
glmFit = glm(default~income+balance,data=Default, family="binomial")
# Display the standard error coefficients
summary(glmFit)$coef[,2]

## ----5.4 6 b-------------------------------------------------------------
# Define a function that takes a dataset and an index as parameters and returns estimates 
# of the income and balance variables
boot.fn = function(data,index){
  fit = glm(default ~ income + balance,data=data,family="binomial",subset=index)
  return(coef(fit))
}

## ----5.4 6 c-------------------------------------------------------------
# Import the library boot
library(boot)

# Call the bootstrap function with Default as the data parameter, the function boot.fn as the statistic parameter and 1000 as number of bootstrap replicates
boot(Default, boot.fn, 1000)

