# 데이터 불러오기
data <- read.csv("loan_data.csv", header=T)
str(data)
data$COMB_COMM <- as.factor(data$COMB_COMM)

# 로지스틱 회귀분석 모델
model <- glm(TARGET ~ . , data=data, family="binomial")
summary(model)

# age, loan_crd_cnt, late_rate, ins_mon_max, call_time, tel_cost_mon, sex, job 제거
library(dplyr)
data <- data %>% select(-AGE,-LOAN_CRD_CNT,-LATE_RATE,-INS_MON_MAX,-CALL_TIME,-TEL_COST_MON,-SEX, -JOB)

model <- glm(TARGET ~ . , data=data, family="binomial")
summary(model)

#train, test set 쪼개기
smp_size <- floor(0.7 * nrow(data))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(data)), size = smp_size)

train_data <- data[train_ind, ]
test_data <- data[-train_ind, ]

train_model <- glm(TARGET ~ . , data=train_data, family="binomial")
summary(train_model)

anova(train_model,test="Chisq")

library(ROCR)
p <- predict(train_model, newdata=test_data, type="response")
pr <- prediction(p, test_data$TARGET)
prf <- performance(pr, measure="tpr", x.measure="fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

temp <- as.data.frame(p)
library(dplyr)

binding <- test_data %>% select(TARGET) %>% mutate(per=temp$p)
binding <- binding %>% mutate(check=ifelse(per>=0.3, 1, 0))
binding <- binding %>% mutate(last=ifelse(TARGET==check, 0, 1))
sum(binding$last)

# cut-off 0.5
1-1181/13016

# cut-off 0.6   -> best
1-1177/13016

# cut-off 0.7
1-1220/13016

# cut-off 0.3
1-1248/13016

# cut-off 0.4
1-1195/13016
