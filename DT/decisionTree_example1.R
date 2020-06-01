#install.packages("rpart")
#install.packages("rpart.plot")
#install.packages("rattle")

library(rpart)
library(rpart.plot)
library(rattle)

#setwd('C:/work/rwork')


# R내장 IRIS데이터 로드
data(iris)
set.seed(415)
str(iris)


# Train & Test data set 구분
idx = sample(1:nrow(iris), 0.7*nrow(iris))
idx

train = iris[idx,]
test = iris[-idx,]

dim(train)
dim(test)

table(train$Species)


# rpart분류모델 생성 및 시각화
model = rpart(Species~., data=train)
model

prp(model)
rpart.plot(model)
fancyRpartPlot(model)


# 모델평가
pred <- predict(model, test)
pred

pred <- predict(model, test, type="class")
pred

table(pred)
table(pred, test$Species)

# accuracy
(14+13+13)/(14+13+4+1+13)