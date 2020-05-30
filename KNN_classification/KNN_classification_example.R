## 예제 : 암(cancer) 샘플 분류(KNN분류알고리즘)

## 단계 1 : 데이터 준비
# 작업디렉토리 setwd('D:/backup/Rdata/R을활용한기계학습/mlr-ko/chapter 3')

wbcd <- read.csv("wisc_bc_data.csv", stringsAsFactors = FALSE)

str(wbcd)

# id 속성 제거
wbcd <- wbcd[-1]

table(wbcd$diagnosis)

wbcd$diagnosis <- factor(wbcd$diagnosis, levels = c("B", "M"),
                         labels = c("Benign", "Malignant"))

round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

# 정규화 함수
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n$area_mean)

wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]


## 3단계 : 데이터로 모델 훈련

library(class)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)


## 4 단계 : 모델 성능 평가

library(gmodels)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)



## 5 단계 : 모델 성능 향상

wbcd_z <- as.data.frame(scale(wbcd[-1]))

summary(wbcd_z$area_mean)

wbcd_train <- wbcd_z[1:469, ]
wbcd_test <- wbcd_z[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test,
                      cl = wbcd_train_labels, k=21)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)

wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=1)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=5)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=11)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=15)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=21)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)

wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k=27)
CrossTable(x = wbcd_test_labels, y = wbcd_test_pred, prop.chisq=FALSE)
