install.packages("xgboost")

library(xgboost)


# 1.데이터 가져오기
#setwd('D:/backup/Rdata/rwork')
weather = read.csv("./data/weather.csv")

head(weather)
dim(weather)
str(weather)

weather_df <- weather[,c(-1,-6,-8,-14)]
str(weather_df)
head(weather_df)
dim(weather_df)

weather_df$RainTomorrow <- ifelse(weather_df$RainTomorrow == "No", 0, 1)
table(weather_df$RainTomorrow)

# 2. train/test 데이터 셋 생성

weather_df

idx <- sample(1:nrow(weather_df), nrow(weather_df)*0.7)
train <- weather_df[idx,]
test <- weather_df[-idx,]

dim(train)
dim(test)

table(train$RainTomorrow)
table(test$RainTomorrow)


# 3. xgb.DMatrix 생성

train_mat <- as.matrix(train[-11])
dim(train_mat)

train_lab <- train$RainTomorrow
length(train_lab)

dtrain <-xgb.DMatrix(data = train_mat, label = train_lab)
dtrain


# 4. model 생성

xgb_model_w <- xgboost(data = dtrain, 
                       max_depth = 2, eta = 1,
                       nthread = 2, nrounds = 2,
                       objective = "binary:logistic", verbose = 0)

xgb_model_w


# 5.

test_mat <- as.matrix(test[-11])
dim(test_mat)
test_lab <- test$RainTomorrow 
length(test_lab) 

# 6. 예측 및 평가
pred_w <- predict(xgb_model_w, test_mat)
range(pred_w) 
cpred_w <- ifelse(pred_w >= 0.5, 1, 0)
table(cpred_w, test_lab)

#test_lab
#cpred_w  0  1
#       0 89 13
#       1  1  7

# Accuracy
(89+7) / length(test_lab)

# mean error
mean_err <- mean(as.numeric(pred_w > 0.5) != test_lab)
cat("test-error =", mean_err)

# feature importance
importance_matrix <- xgb.importance(colnames(train_mat), model = xgb_model_w)
importance_matrix
