install.packages("glmnet")
library(glmnet)



swiss <- datasets::swiss
x <- model.matrix(Fertility~., swiss)[,-1]
y <- swiss$Fertility

set.seed(1575)
train = sample(1:nrow(x), nrow(x)/2)
test = (-train)
ytest = y[test]

cv.lasso <- cv.glmnet(x[train,], y[train], alpha=1)
lasso.coef = predict(cv.lasso, type = "coefficients", s=cv.lasso$lambda.min) # coefficients
lasso.prediction = predict(cv.lasso, s=cv.lasso$lambda.min, newx = x[test,]) # coefficients

#--------------------------------
data <- read.csv("ToyotaCorolla.csv", header=T)

data_x_scaled <- scale(data_x, center = TRUE, scale = TRUE)

library(dplyr)
data_x
x <- model.matrix(Price~., data_x)[,-1]
y <- data_x$Price
set.seed(1575)

train1 = sample(1:nrow(x), 1000)
train2 = sample(1:nrow(x),1000)
train3 = sample(1:nrow(x),1000)
train4 = sample(1:nrow(x),1000)
train5 = sample(1:nrow(x),1000)
train6 = sample(1:nrow(x),1000)
train7 = sample(1:nrow(x),1000)

library(glmnet)
cv.lasso1 <- cv.glmnet(x[train1,], y[train1], alpha=1)
lasso.coef1 = predict(cv.lasso1, type = "coefficients", s=cv.lasso1$lambda.min) # coefficients

cv.lasso2 <- cv.glmnet(x[train2,], y[train2], alpha=1)
lasso.coef2 = predict(cv.lasso2, type = "coefficients", s=cv.lasso2$lambda.min) # coefficients

cv.lasso3 <- cv.glmnet(x[train3,], y[train3], alpha=1)
lasso.coef3 = predict(cv.lasso3, type = "coefficients", s=cv.lasso3$lambda.min) # coefficients

cv.lasso4 <- cv.glmnet(x[train4,], y[train4], alpha=1)
lasso.coef4 = predict(cv.lasso4, type = "coefficients", s=cv.lasso4$lambda.min) # coefficients

cv.lasso5 <- cv.glmnet(x[train5,], y[train5], alpha=1)
lasso.coef5 = predict(cv.lasso5, type = "coefficients", s=cv.lasso5$lambda.min) # coefficients

cv.lasso6 <- cv.glmnet(x[train6,], y[train6], alpha=1)
lasso.coef6 = predict(cv.lasso6, type = "coefficients", s=cv.lasso6$lambda.min) # coefficients

cv.lasso7 <- cv.glmnet(x[train7,], y[train7], alpha=1)
lasso.coef7 = predict(cv.lasso7, type = "coefficients", s=cv.lasso7$lambda.min) # coefficients

cardata <- read.csv("cardata.csv", header=TRUE)
boxplot(cardata)
#--------------------------------------
cor(data_x)

result2 <- read.csv("toyota221.csv",header=TRUE)
boxplot(result2)
result <- read.csv("toyotaassignment2.csv", header=TRUE)
boxplot(result)
# 상관관계 높은 것  = price, age, km
# 상관관계 없는 것  = age , mfg_month, hp, cc, doors,gurarantee


sampledata <- read.csv("toyota.csv", header=TRUE)

x <- model.matrix(Price~., sampledata)[,-1]
y <- data_x$Price
set.seed(1575)

train1 = sample(1:nrow(x), 200)
train2 = sample(1:nrow(x),200)
train3 = sample(1:nrow(x),200)
train4 = sample(1:nrow(x),200)
train5 = sample(1:nrow(x),200)
train6 = sample(1:nrow(x),200)
train7 = sample(1:nrow(x),200)

cor(sampledata)
library(glmnet)
cv.lasso1 <- cv.glmnet(x[train1,], y[train1], alpha=1)
lasso.coef1 = predict(cv.lasso1, type = "coefficients", s=cv.lasso1$lambda.min) # coefficients

cv.lasso2 <- cv.glmnet(x[train2,], y[train2], alpha=1)
lasso.coef2 = predict(cv.lasso2, type = "coefficients", s=cv.lasso2$lambda.min) # coefficients

cv.lasso3 <- cv.glmnet(x[train3,], y[train3], alpha=1)
lasso.coef3 = predict(cv.lasso3, type = "coefficients", s=cv.lasso3$lambda.min) # coefficients

cv.lasso4 <- cv.glmnet(x[train4,], y[train4], alpha=1)
lasso.coef4 = predict(cv.lasso4, type = "coefficients", s=cv.lasso4$lambda.min) # coefficients

cv.lasso5 <- cv.glmnet(x[train5,], y[train5], alpha=1)
lasso.coef5 = predict(cv.lasso5, type = "coefficients", s=cv.lasso5$lambda.min) # coefficients

cv.lasso6 <- cv.glmnet(x[train6,], y[train6], alpha=1)
lasso.coef6 = predict(cv.lasso6, type = "coefficients", s=cv.lasso6$lambda.min) # coefficients

cv.lasso7 <- cv.glmnet(x[train7,], y[train7], alpha=1)
lasso.coef7 = predict(cv.lasso7, type = "coefficients", s=cv.lasso7$lambda.min) # coefficients
