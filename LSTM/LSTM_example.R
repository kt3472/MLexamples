#install.packages("keras")
#install.packages("tensorflow")
#install_tensorflow()

library(keras)
library(zoo)
library(dplyr)
library(ggplot2)
library(tensorflow)

# https://data.oecd.org/ OECD

#setwd("D:/backup/Rdata/rwork/LSTM")

df <- read.csv("./data/korea_interest_rate(long_term).csv")

head(df)
str(df)
head(df$TIME)

df$date <- format(df$TIME, format="%b-%Y")
df$date <- as.Date(df$TIME)
head(df$date)
class(df$date)

df_rate <- data.frame(df$date, df$Value)
ggplot(df_rate, aes(x= df.date, y = df.Value))+geom_line()

df_rate[,2]

N = length(df_rate[,2])
N
step <- 1

a = as.numeric(df_rate[,2])
a = c(a, replicate(step, tail(a, 1)))

x = NULL
y = NULL

for(i in 1:N){
  s = i-1+step
  x = rbind(x,a[i:s])
  y = rbind(y,a[s+1])
}

cbind(head(x), head(y))


X = array(x, dim=c(N, step,1))

model = keras_model_sequential() %>%   
  layer_lstm(units=128, input_shape=c(step, 1), activation="relu") %>%  
  layer_dense(units=64, activation = "relu") %>%  
  layer_dense(units=32) %>%  
  layer_dense(units=1, activation = "linear")

model %>% compile(loss = 'mse',
                  optimizer = 'adam',
                  metrics = list("mean_absolute_error")
)

model %>% summary()

model %>% fit(X,y, epochs=50, batch_size=32, shuffle = FALSE)
y_pred = model %>% predict(X)
y_pred

scores = model %>% evaluate(X, y, verbose = 0)
print(scores)

x_axes = seq(1:length(y_pred))
plot(x_axes, y, type="l", col="red", lwd=2)
lines(x_axes, y_pred, col="blue",lwd=2)
legend("topleft", legend=c("y-original", "y-predicted"),
       col=c("blue", "red"), lty=1,cex=0.8) 
