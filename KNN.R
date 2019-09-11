# wine (classification)
library(dplyr)
data <- read.csv("wine1.csv", header=TRUE)
data <- data[,c(1:14)]
data <- na.omit(data)

str(data)
data_x <- data[,-1]
data_y <- data$Class
data_y <- as.factor(data$Class)
data_y_frame <- as.data.frame(data_y)
colnames(data_y_frame) = c("Type")
data_x_scaled <- scale(data_x, center = TRUE, scale = TRUE)
data_xy_scaled <- cbind(data_x_scaled, data_y_frame)

set.seed(415)
idx=sample(1:nrow(data_xy_scaled),0.7*nrow(data_xy_scaled))
data_xy_scaled_train = data_xy_scaled[idx,]
data_xy_scaled_test = data_xy_scaled[-idx,]


train_x <- data_xy_scaled_train[,-14]
train_y <- data_xy_scaled_train[,14]
test_x <- data_xy_scaled_test[,-14]
test_y <- data_xy_scaled_test[,14]

cm <- table(data_pred,test_y)
accuracy_wine <- sum(data_pred == test_y) / length(test_y)
wine_k_result[i] <- accuracy_wine
wine_k_result <- numeric()
data_pred <- knn(train_x, test_x, train_y, k=1)
library(class)
for(i in 1:10){
data_pred <- knn(train_x, test_x, train_y, k=i)
accuracy_wine <- sum(data_pred == test_y) / length(test_y)
wine_k_result[i] <- accuracy_wine

}
wine_result <- data.frame(wine_k_result)
ggplot(wine_result,aes(x=1:10, y=wine_k_result))+geom_line()
---------------------------

------------------------------
# 분류 정확도
49/54
library(kknn)
knntr <- train.kknn(data_xy_scaled_train$Type ~ ., data_xy_scaled_train, kmax=10, distance=2, kernel="rectangular")
summary(knntr)

lm <- lm(data_toyota_y~., data=data_toyota_scaled_train)
summary(lm)
library(ggplot2)
plot(formula=Age_08_04~KM,
     data = data_toyota_scaled_train,
     col = alpha(c("purple", "blue", "green"), 0.7)[data_toyota_scaled_train$data_toyota_y],
     main = "KNN (k = 1)")

points(formula = Age_08_04 ~ KM,
       data = data_toyota_scaled_test,
       pch = 17,
       cex = 1.2,
       col = alpha(c("purple", "blue", "green"), 0.7)[data_pred])

legend("topright",
       c(paste("train", levels(data_xy_scaled_train$Type)), paste("test", levels(data_xy_scaled_test$Type))),
       pch = c(rep(1, 3), rep(17, 3)),
       col = c(rep(alpha(c("purple", "blue", "green"), 0.7), 2)),
       cex = 0.9)

accuracy_21 <- sum(data_pred == test_y) / length(test_y)

#시각화
library(caret)

decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 100, showgrid = TRUE, ...) {
  
  if(!is.null(class)) cl <- data[,class] else cl <- 1
  data <- data[,1:2]
  k <- length(unique(cl))
  
  plot(data, col = as.integer(cl)+1L, pch = as.integer(cl)+1L, ...)
  
  # make grid
  r <- sapply(data, range, na.rm = TRUE)
  xs <- seq(r[1,1], r[2,1], length.out = resolution)
  ys <- seq(r[1,2], r[2,2], length.out = resolution)
  g <- cbind(rep(xs, each=resolution), rep(ys, time = resolution))
  colnames(g) <- colnames(r)
  g <- as.data.frame(g)
  
  ### guess how to get class labels from predict
  ### (unfortunately not very consistent between models)
  p <- predict(model, g, type = predict_type)
  if(is.list(p)) p <- p$class
  p <- as.factor(p)
  
  if(showgrid) points(g, col = as.integer(p)+1L, pch = ".")
  
  z <- matrix(as.integer(p), nrow = resolution, byrow = TRUE)
  contour(xs, ys, z, add = TRUE, drawlabels = FALSE,
          lwd = 2, levels = (1:(k-1))+.5)
  
  invisible(z)
}
data_x_frame <- as.data.frame(data_x_scaled)
pca_dt <- prcomp(data_x_frame, center = T, scale. = T)
summary(pca_dt)
pca_dt$x

x_vari <- data.frame(pca_dt$x[,1],pca_dt$x[,2],data_ynew$data_ynew)
colnames(x_vari) <- c("PCA1","PCA2","Wine")

model_1 <- knn3(Wine ~ ., data=x_vari, k = 1)
model_10 <- knn3(Wine ~ ., data=x_vari, k = 10)
model_50 <- knn3(Wine ~ ., data=x_vari, k = 50)
decisionplot(model_1, x_vari, class = "Wine", main = "KNN K=1")
decisionplot(model_10, x_vari, class = "Wine", main = "KNN K=10")
decisionplot(model_50, x_vari, class = "Wine", main = "KNN K=50")


#59, 130
data_ynew <- numeric()
data_ynew[1:59] <- 1
data_ynew[60:130] <- 2
data_ynew[131:178] <- 3
data_ynew <- as.data.frame(data_ynew)
data_ynew$data_ynew <- as.factor(data_ynew$data_ynew)

# 수치예측
library(dplyr)
data_toyota <- read.csv("ToyotaCorolla_Simple.csv", header=TRUE)
data_toyota <- data_toyota[,c(1:10)]
data_toyota <- data_toyota %>% filter(!cc>10000)
data_toyota <- na.omit(data_toyota)
data_toyota <- data_toyota[,c(3:10)]

data_toyota_x <- data_toyota[,c(1:7)]
data_toyota_y <- data_toyota[,8]
toyota_scaled <- scale(data_toyota_x, center = TRUE, scale = TRUE)
data_toyota_scaled <- cbind(toyota_scaled, data_toyota_y)
data_toyota_scaled <- as.data.frame(data_toyota_scaled)
data_toyota_scaled_y <- data_toyota_scaled[,8]
data_toyota_scaled_x <- data_toyota_scaled[,-8]

install.packages("FNN")
library(FNN)

set.seed(1234)
idx=sample(1:nrow(data_toyota_scaled),0.7*nrow(data_toyota_scaled))
data_toyota_scaled_train = data_toyota_scaled[idx,]
data_toyota_scaled_test = data_toyota_scaled[-idx,]


train_x <- data_toyota_scaled_train[,-8]
train_y <- data_toyota_scaled_train[,8]
test_x <- data_toyota_scaled_test[,-8]
test_y <- data_toyota_scaled_test[,8]

library(class)
toyota_data_pred <- knn.reg(train_x, test_x, train_y, k=3, algorithm="kd_tree")
result_rmse <- numeric()
result_mae <- numeric()
result_mape <- numeric()
for(i in 1:10){
  toyota_data_pred <- knn.reg(train_x, test_x, train_y, k=i, algorithm="kd_tree")
  result_rmse[i] <- sqrt(mean((toyota_data_pred$pred-test_y)^2))
  # MAE
  result_mae[i] <- mean(abs(toyota_data_pred$pred - test_y))
  # MAPE
  result_mape[i] <- 100*mean(abs((toyota_data_pred$pred - test_y)/test_y))
}

k <- list(1:10)
kmse <- data.frame(k,result_rmse)
kmae <- data.frame(k, result_mae)
kmape <- data.frame(k, result_mape)
ggplot(kmse, aes(x=1:10, y=result_rmse))+geom_line()
ggplot(kmae, aes(x=1:10, y=result_mae))+geom_line()
ggplot(kmape, aes(x=1:10, y=result_mape))+geom_line()

# 시각화 (수치예측)
pca_toyota <- prcomp(data_toyota_scaled_x, center = T, scale. = T)
summary(pca_toyota)
pca_toyota$x

x_toyota_vari <- data.frame(pca_toyota$x[,1],pca_toyota$x[,2],data_toyota_scaled_y)
colnames(x_toyota_vari) <- c("PCA1","PCA2","toyotaprice")
x_toyota_vari$toyotaprice <- as.factor(x_toyota_vari$toyotaprice)
str(x_toyota_vari)

model_1 <- knn3(toyotaprice ~ ., data=x_toyota_vari, k = 1)
model_10 <- knn3(toyotaprice ~ ., data=x_toyota_vari, k = 10)
model_50 <- knn3(toyotaprice ~ ., data=x_toyota_vari, k = 50)
model_1_reg <- knn.reg(train_x, test_x, train_y, k=3, algorithm="kd_tree")
decisionplot(model_1, x_toyota_vari, class = "toyotaprice", main = "KNN K=1")
decisionplot(model_10, x_toyota_vari, class = "toyotaprice", main = "KNN K=10")
decisionplot(model_50, x_toyota_vari, class = "toyotaprice", main = "KNN K=50")

# 2번문제 (거리 계산 방식 변환)
install.packages("knnGarden")
library(knnGarden)
accuracy_manhattan <- numeric()

for(i in 1:20){
manhattan <- knnVCN(train_x, train_y, test_x, K=i, ShowObs=F, method="manhattan")
accuracy_manhattan[i] <- sum(manhattan$TstXIBelong == test_y) / length(test_y)
}
accuracy_manhattan
compare <- data.frame(manhattan$TstXIBelong, test_y)
compare <- compare %>% mutate(comp = ifelse(manhattan.TstXIBelong==test_y,0,1))

accuracy_manhattan_frame <- data.frame(accuracy_manhattan)
ggplot(accuracy_manhattan_frame, aes(x=1:20, y=accuracy_manhattan))+geom_line()
mahalanobis <- knnMCN(train_x, train_y, test_x, K=10, ShowObs=F)


