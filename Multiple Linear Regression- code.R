bikedata <- read.csv("hour.csv", header = T)
data <- read.csv("hour.csv", header = T)
hist(bikedata$temp)
hist(bikedata$atemp)
hist(bikedata$hum)
hist(bikedata$windspeed)
bikeseasontemp <- data %>% group_by(mnth) %>% summarize(humidity= mean(hum))

library(dplyr)
library(ggplot2)
install.packages("ggplot2")
bikemonthtemp <- bikedata1 %>% group_by(workingday) %>% summarize(count = sum(registered))
barplot(bikemonthtemp$count)

ggplot(bikemonthtemp, aes(workingday,count)) + geom_bar(stat='identity', color='black', fill='gray') + ggtitle("회원 근무일 유무에 따른 자전거 대여량")


#2012 > 2011  workingday >breaktimeday     casual - workingday no difference   registered workingday many
sum(bikedata$cnt)
bike <- data %>% group_by(holiday) %>% summarize(count=n_distinct(dteday))



ggplot() + geom_bar(data=bike, aes(x=mnth,y=count), stat='identity')


#bikedata 범주형 변수 factor 로 변환
library(ggplot2)
bikedata <- bikedata %>% select(-c(registered))
str(bikedata)
bikedata$season <- as.factor(bikedata$season)
bikedata$yr <- as.factor(bikedata$yr)
bikedata$mnth <- as.factor(bikedata$mnth)
bikedata$hr <- as.factor(bikedata$hr)
bikedata$holiday <- as.factor(bikedata$holiday)
bikedata$weekday <- as.factor(bikedata$weekday)
bikedata$weathersit <- as.factor(bikedata$weathersit)

# 다중회귀분석  logmodel
logmodel <- lm(log(cnt)~. , data=bikedata)
model <- lm(cnt~. , data=bikedata)

#summary, plot, 신뢰구간
summary(model)
plot(model)
confint.lm(logmodel)

qqnorm(log(bikedata$cnt))

#train, test set 쪼개기
sample_num <- sample(1:nrow(bikedata), size=round(0.2*nrow(bikedata)))
test_bikedata <- bikedata[sample_num,]
train_bikedata <- bikedata[-sample_num,]

ind <- sample(x=2, nrow(bikedata),replace=T, prob=c(0.7,0.3))
traindata <- bikedata[ind==1,]
testdata <- bikedata[ind==2,]

#train set의 다중회귀분석모델 그리고 test set을 통한 검증
train_logmodel <- lm(log(cnt)~. , data=train_bikedata)
pred <- predict(train_logmodel, test_bikedata)
cor(pred, test_bikedata$cnt)

hist(log(testdata$cnt))
hist(test_logmodel)
test_logmodel <- as.data.frame(test_logmodel)
confusionMatrix(test_logmodel, testdata$cnt)
na.omit(test_bikedata)
test_logmodel <- as.numeric(test_logmodel)
str(test_logmodel)                
anova(logmodel)

