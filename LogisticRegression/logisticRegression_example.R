
## 로지스틱 회귀분석 


# 단계1. 데이터 가져오기
#setwd("D:\backup\rwork\Part-IV")
weather = read.csv("weather.csv", stringsAsFactors = F) 
dim(weather)  # 366  15
head(weather)
str(weather)

weather_df <- weather[, c(-1, -6, -8, -14)]
str(weather_df)

weather_df$RainTomorrow[weather_df$RainTomorrow=='Yes'] <- 1
weather_df$RainTomorrow[weather_df$RainTomorrow=='No'] <- 0
weather_df$RainTomorrow <- as.numeric(weather_df$RainTomorrow)
head(weather_df)

#  단계2.  데이터 샘플링
idx <- sample(1:nrow(weather_df), nrow(weather_df)*0.7)
train <- weather_df[idx, ]
test <- weather_df[-idx, ]
dim(train) # 256  11(10(x) - > 1(y))

#  단계3.  로지스틱  회귀모델 생성 : 학습데이터 
weater_model <- glm(RainTomorrow ~ ., data = train, family = 'binomial')
weater_model 
summary(weater_model) 


# 단계4. 로지스틱  회귀모델 예측치 생성 : 검정데이터 
# newdata=test : 새로운 데이터 셋, type="response" : 0~1 확률값으로 예측 
pred <- predict(weater_model, newdata=test, type="response")  
pred 
str(pred) # 비율예측 
range(pred, na.rm = T) # 0.0004361545 0.9912157848

# Sigmoid 함수 : cut off = 0.5
spred <- ifelse(pred >= 0.5, 1, 0)
table(spred)
#spred
# 0  1 
#94 14

# 단계5 : model 평가 
# confusion matrix
table(spred, test$RainTomorrow)# 예측치(row) vs 관측치(column) 
#spred  0  1
#    0 84 10
#    1  7  7

# 분류정확도 
(84+7) / nrow(test) # 0.8272727

# 오분류 
(10+7) / nrow(test) #  0.1545455

# NO인 경우 
84 / (84+7) # 0.9230769
# Yes인 경우 
7 / (10+7) # 0.4117647


### ROC Curve를 이용한 모형평가(분류정확도)  ####
# Receiver Operating Characteristic

install.packages("ROCR")
library(ROCR)

# ROCR 패키지 제공 함수 : prediction() -> performance
pr <- prediction(pred, test$RainTomorrow)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)


# model 결과 변수 확인 
names(weater_model)
weater_model$fitted.values[1] # 예측치 
weater_model$y[1] # 관측치(정답)
weater_model$residuals[1] # 오차 

# 로지스틱 회귀모형 검정 
install.packages("ResourceSelection")
library(ResourceSelection)

hoslem.test(weater_model$y, 
            weater_model$fitted.values)
# p-value = 0.3784 >= 0.05
# X-squared = 8.5863, df =  8, p-value = 0.3784