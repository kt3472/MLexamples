#install.packages("randomForest")
#install.packages("e1071")

library(randomForest)
library(caret)
library(e1071)

# 1. 데이터 로드
# UniversalBank.csv : 개인 대출 제안에 대한 수락 여부 데이터
# Personal.Loan(수락여부) 1: 수락  2:거절
data = read.csv('./data/UniversalBank.csv')
str(data)


# 2. 데이터 탐색 및 전처리
data <- data[c(-1,-5)]
str(data)

data$Education <- as.factor(data$Education)
data$Personal.Loan <- as.factor(data$Personal.Loan)
data$Securities.Account <- as.factor(data$Securities.Account)
data$CD.Account <- as.factor(data$CD.Account)
data$Online <- as.factor(data$Online)
data$CreditCard <- as.factor(data$CreditCard)

str(data)
table(data$Personal.Loan)


# 3. train / test data set 생성(8:2)
idx = sample(1:nrow(data), 0.8*nrow(data))
idx

train = data[idx,]
test = data[-idx,]


# 4. RandomForest 모델 생성 및 중요 변수 시각화
data_rm <- randomForest(Personal.Loan ~.,
                        data = train,
                        ntree = 400,
                        importance =T,
                        mtry=3,
                        na.action=na.omit)
data_rm

data_rm$err.rate
t<-data_rm$confusion
t

varImpPlot(data_rm)


# 5. 모델평가
pred = predict(data_rm, test)
pred

confusionMatrix(pred, test$Personal.Loan)



# 최적 파라미터 찾기(ntree, mtry)
ntree<-c(400,500,600)
mtry <- c(2:4)

param <- data.frame(n=ntree, m=mtry)

for (i in param$n){
  cat('ntree = ',i, '\n')
  
  for (j in param$m){
    
    cat('mtry=', j, '\n')
    
    model = randomForest(Personal.Loan~., data = train,
                         ntree = i, mtry=j,
                         na.action = na.omit)
    
    t <- model$confusion
    
    acc <- (t[1,1]+t[2,2])/nrow(data)

    print(acc)
    
  }
  
}


# 데이터 분할(메모리분할사용)
#install.packages("foreach")
library(foreach)

data_rm_foreach <- foreach(i= rep(100, 5), .combine = combine) %do% 
  randomForest(Personal.Loan~., data=data, 
               ntree=i, mtry=2, na.action=na.omit )


data_rm_foreach
data_rm