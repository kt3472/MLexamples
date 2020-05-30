## 1. 단순회귀분석 


product <- read.csv("product.csv", header=TRUE)
head(product) # 친밀도 적절성 만족도(등간척도 - 5점 척도)

str(product) # 'data.frame':  264 obs. of  3 variables:
y = product$제품_만족도 # 종속변수
x = product$제품_적절성 # 독립변수
df <- data.frame(x, y)
df

# 회귀모델 생성 
result.lm <- lm(formula=y ~ x, data=df)
result.lm # 회귀계수 

x = 4
Y = 0.7789 + 0.7393 * x
Y # 3.7361 : 예측치 

head(df, 1) # x=4, y=3
y = 3
Y - y # 0.7361 - 오차(잔차)

names(result.lm) # "coefficients"  "residuals" "fitted.values"
result.lm$coefficients
result.lm$residuals[1] # -0.735963 
result.lm$fitted.values[1] # 3.735963

# 모델에 의해서 예측한 Y값 
fitted.values(result.lm)[1:2] # 3.735963 2.996687

# 회귀모델 예측 : 친밀도(설명변수) 5인 경우 -> 만족도(반응변수) 예측
# 형식) predict(model, 설명변수)
predict(result.lm, data.frame(x=5) ) # 3.921361 
predict(result.lm, data.frame(x=1) ) # 2.325461 

# (2) 선형회귀 분석 결과 보기
summary(result.lm)
# 1. F 검정 : p-value < 0.05(알파)
# 2. model 설명력 : 0.58
# 3. x변수 유의성 검정 


# (3) 단순선형회귀 시각화
# x,y 산점도 그리기 
plot(formula=y ~ x, data=df)
# 회귀분석
result.lm <- lm(formula=y ~ x, data=df)
# 회귀선 
abline(result.lm, col='red')



## 2. 다중회귀분석

product <- read.csv("C:/Rwork/Part-IV/product.csv", header=TRUE)
head(product) # 친밀도 적절성 만족도(등간척도 - 5점 척도)


#(1) 적절성 + 친밀도 -> 만족도  
y = product$제품_만족도 # 종속변수
x1 = product$제품_친밀도 # 독립변수2
x2 = product$제품_적절성 # 독립변수1

df <- data.frame(x1, x2, y)
head(df, 1) # 3  4 3

result.lm <- lm(formula=y ~ x1 + x2, data=df)
#result.lm <- lm(formula=y ~ ., data=df)

# 계수 확인 
result.lm
x1=3; x2=4
Y = 0.66731 + 0.09593*x1 + 0.68522*x2
Y # 예측치 

# 오차(Error)=예측치-관측치(정답)
3.69598 - 3 # 0.69598

result.lm$residuals[1:264] # -0.6959802
result.lm$fitted.values[1:264] # 3.69598
dim(df) # 264   3

summary(result.lm)
# 1. F-statistic: 193.8(-1.96~+1.96), p-value < 0.05
# 2. R-squared:  0.5945
