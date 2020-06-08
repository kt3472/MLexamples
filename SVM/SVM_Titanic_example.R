library(e1071)
library(dplyr)

#setwd("D:/backup/Rdata/rwork/SVM")

train<-read.csv("./data/titanic_train.csv")
test<-read.csv("./data/titanic_test.csv")
dim(train)
dim(test)

head(train)
str(train)

head(test)
str(test)


apply(train,2,function(x)length(which(is.na(x))))
apply(test,2,function(x)length(which(is.na(x))))


train$Age <- ifelse(is.na(train$Age),
                         ave(train$Age, FUN = function(x) mean(x, na.rm = TRUE)), train$Age)

test$Age <- ifelse(is.na(test$Age),
                   ave(test$Age, FUN = function(x) mean(x, na.rm = TRUE)), test$Age)

train$Fare <- ifelse(is.na(train$Fare),
                    ave(train$Fare, FUN = function(x) mean(x, na.rm = TRUE)), trainset$Fare)

test$Fare <- ifelse(is.na(test$Fare),
                      ave(test$Fare, FUN = function(x) mean(x, na.rm = TRUE)), test$Fare)


train$Survived <- factor(train$Survived, levels = c(0, 1))

train$Sex <- factor(train$Sex,
                         levels = c('male', 'female'),
                         labels = c(1, 2))
train$Pclass <- factor(train$Pclass, levels = c(1, 2, 3))


test$Sex <- factor(test$Sex,
                     levels = c('male', 'female'),
                     labels = c(1, 2))

# train$Embarked의 factor는 " "을 포함하여 4개 이므로 test의 Emabarked의 factor도 동일하게 4개로 만들어줌
test$Embarked <- factor(test$Embarked,
                          levels = c('C', 'Q', 'S',''),
                          labels = c(1, 2, 3,4))
test$Pclass <- factor(test$Pclass, levels = c(1, 2, 3))

str(train)
str(test)

train[c(6, 7, 8, 10)] = scale(train[c(6, 7, 8, 10)])
test[c(5, 6, 7, 9)] = scale(test[c(5, 6, 7, 9)])

str(train)
str(test)

train_set <- train[c(2,3,5,6,7,8,10,12)]
test_set <- test[c(2,4,5,6,7,9,11)]

str(train_set)
str(test_set)

svm_classifier = svm(formula = Survived ~ .,
                 data = train_set,
                 type = 'C-classification',
                 kernel = 'radial')


svm_classifier


y_pred = predict(svm_classifier, newdata = test_set)


write.csv(y_pred,file = './data/predicts_titanic.csv')
