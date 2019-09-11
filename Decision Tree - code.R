data <- read.csv("wine.csv", header=TRUE)

install.packages("rpart")
install.packages("party")
library(rpart)
library(party)
?rpart
?ctree
partymod <- ctree(Class ~. , data=data)
plot(partymod)

rpartmod <- rpart(Class ~. , data=data, method="class")
plot(rpartmod)
text(rpartmod)

printcp(rpartmod)
plotcp(rpartmod)
ptree<-prune(rpartmod, cp= rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]),"CP"])
plot(ptree)
text(ptree)

library(caret)
set.seed(1234)
intrain <- createDataPartition(y=data$Class, p=0.7, list=FALSE)
train <- data[intrain,]
test <- data[-intrain,]

rpartmod <- rpart(Class ~. , data=train, method="class")
plot(rpartmod)
text(rpartmod)
printcp(rpartmod)
plotcp(rpartmod)
ptree<-prune(rpartmod, cp= rpartmod$cptable[which.min(rpartmod$cptable[,"xerror"]),"CP"])
plot(ptree)
text(ptree)

install.packages("e1071")
library(e1071)
rpartpred <- predict(ptree, test, type='class')
confusionMatrix(rpartpred, test$Class)

?predict

rpart_md_gini <- rpart(Class~., data=train, parms = list(split = "gini"), method="class")
plot(rpart_md_gini)
text(rpart_md_gini, use.n= TRUE)
rpartpredgini <- predict(rpart_md_gini, test, type='class')
confusionMatrix(rpartpredgini, test$Class)

rpart_md_info <- rpart(Class~., data=train, parms = list(split = "information"), method="class")
plot(rpart_md_info)
text(rpart_md_info, use.n= TRUE)
rpartpredinfo <- predict(rpart_md_info, test, type='class')
confusionMatrix(rpartpredinfo, test$Class)

