
#install.packages("e1071")
library(e1071)

data(iris)

set.seed(2020)

idx <- sample(1:nrow(iris), 0.7*nrow(iris))

train <- iris[idx,]
dim(train)

test <- iris[-idx,]
dim(test)

model_svm <- svm(Species ~., data = train, na.action= na.omit)
summary(model_svm)


pred <- predict(model_svm, test)
table(pred, test$Species)

# Results
#pred         setosa versicolor virginica
#setosa         15          0         0
#versicolor      0         14         1
#virginica       0          1        14