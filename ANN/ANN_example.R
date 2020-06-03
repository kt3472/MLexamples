
#install.packages('neuralnet')
library(neuralnet)

# 1.데이터 로드(R내장 iris데이터)
data("iris")

str(iris)
summary(iris)

# 2.train / test data set 생성(8:2)
set.seed(2020)
idx <- sample(1:nrow(iris), nrow(iris)*0.8)

train_iris = iris[idx,]
test_iris = iris[-idx,]

dim(train_iris)
dim(test_iris)


# 3.factor형인 Species컬럼을 numeric 형으로 변경

train_iris$Species <- ifelse(train_iris$Species =='setosa',1,
                                ifelse(train_iris$Species == 'versicolor',2,3))
str(train_iris)

test_iris$Species <- ifelse(test_iris$Species =='setosa',1,
                             ifelse(test_iris$Species == 'versicolor',2,3))
str(test_iris)


# 4.0~1 범위로 정규화
normal <- function(x){
  return((x-min(x))/(max(x)-min(x)))
}

train_nor <- as.data.frame(lapply(train_iris, normal))
summary(train_nor)
str(train_nor)

test_nor <- as.data.frame(lapply(test_iris, normal))
summary(test_nor)
str(test_nor)


# 5-1.NN알고리즘모델링(hidden layer = 1) 및 평가
model_net = neuralnet(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
                      data=train_nor, hidden = 1, threshold = 0.01,
                      linear.output=FALSE, act.fct = 'logistic') 

model_net
names(model_net)
model_net$net.result
model_net$weights

plot(model_net)

model_result <- compute(model_net, test_nor[-5])
names(model_result)
model_result$net.result 
range(model_result$net.result)

# 상관분석
cor(model_result$net.result, test_nor$Species)
# MSE(Mean Squared Error)
mean((model_result$net.result- test_nor$Species)**2)


# 5-2.NN알고리즘모델링(hidden layer = 2, 역전파(backpropergation)적용) 및 평가

model_net2 = neuralnet(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
                       data=train_nor, hidden = 2, learningrate=0.01,
                       linear.output=FALSE, act.fct = 'logistic',
                       algorithm="backprop") 
plot(model_net2)

model_result2 <- compute(model_net2, test_nor[c(1:4)])
cor(model_result2$net.result, test_nor$Species)  
mean((model_result2$net.result- test_nor$Species)**2)


# 5-3.NN알고리즘모델링(다층퍼셉트론(multi-layer), 역전파(backpropergation)적용) 및 평가

model_net3 = neuralnet(Species ~ Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, 
                       data=train_nor, hidden = c(2, 2), learningrate=0.01,
                       linear.output=FALSE, act.fct = 'logistic',
                       algorithm="backprop") 
plot(model_net3)

model_result3 <- compute(model_net3, test_nor[c(1:4)])
cor(model_result3$net.result, test_nor$Species)
mean((model_result3$net.result- test_nor$Species)**2)
