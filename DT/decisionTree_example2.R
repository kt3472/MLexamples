
library(rpart)
library(rpart.plot)
library(rattle)

# 1. 데이터 로드
# wdbc_data.csv : 유방암 진단결과 데이터 셋
wdbc <- read.csv("./data/wdbc_data.csv", stringsAsFactors = FALSE)
str(wdbc)


# 2. 데이터 탐색 및 전처리
wdbc <- wdbc[-1]
head(wdbc)
head(wdbc[,c('diagnosis')], 10)
table(wdbc$diagnosis)
wdbc$diagnosis <- factor(wdbc$diagnosis, levels = c("B","M"))
wdbc$diagnosis[1:10]


# 3. 데이터 정규화

normalize <-function(x) {
  return ((x-min(x))/(max(x)-min(x)))
}

wdbc_x <- as.data.frame(lapply(wdbc[2:31], normalize))
wdbc_x

dim(wdbc_x)
summary(wdbc_x)
class(wdbc_x)

wdbc_df <- data.frame(wdbc$diagnosis, wdbc_x)
dim(wdbc_df)
head(wdbc_df)


# 4. train / test data set 생성(7:3)
set.seed(415)
idx = sample(nrow(wdbc_df), 0.7*nrow(wdbc_df))
wdbc_train = wdbc_df[idx, ]
wdbc_test = wdbc_df[-idx, ]


# 5. Decision Tree 모델 생성 및 시각화
model_wdbc = rpart(wdbc.diagnosis~.,data=wdbc_train)
model_wdbc

prp(model_wdbc)
rpart.plot(mode_wdbc)
fancyRpartPlot(model_wdbc)


# 6. 모델 평가(Accuracy, Precision, Recall, F1 score)

pred <- predict(model_wdbc, wdbc_test)
head(pred)
pred <- predict(model_wdbc, wdbc_test, type="class")
head(pred)

table(pred)
table(pred, wdbc_test$wdbc.diagnosis)

# 정확도(accuracy)
results <- table(pred, wdbc_test$wdbc.diagnosis)
accuracy1 <- (results[1,1]+results[2,2])/nrow(wdbc_test)
accuracy1

# 정확률(precision)
precision <- results[1,1]/sum(results[1,])
precision

# 재현율(recall)
recall <- results[1,1] / sum(results[,1])
recall

# F1 Score
f1 <- 2/((1/precision)+(1/recall))
f1

