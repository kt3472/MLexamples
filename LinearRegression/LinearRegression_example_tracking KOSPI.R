
#setwd('C:/work/kospi_kospi200')

kospi_kospi200 <- read.csv("./data/kospi_kospi200.csv", header = TRUE)

head(kospi_kospi200)

str(kospi_kospi200)

kospi <- kospi_kospi200$KOSPI
kospi200 <- kospi_kospi200$KOSPI200
kospi200ex <- kospi_kospi200$KOSPI200ex

df <- data.frame(kospi200, kospi200ex,kospi)

head(df,5)

result.lm <- lm(formula = kospi ~ kospi200 + kospi200ex, data = df)

result.lm

summary(result.lm)


df$kospi.chg <- C(-diff(df$kospi)/df$kospi[-1] * 100, NA)

df$kospi.chg <- c(NA, diff(df$kospi)/df$kospi[-1]*100)
df$kospi200.chg <- c(NA, diff(df$kospi200)/df$kospi200[-1]*100)
df$kospi200ex.chg <- c(NA, diff(df$kospi200ex)/df$kospi200ex[-1]*100)

head(df)


result.lmchg <- lm(formula = kospi.chg ~ kospi200.chg + kospi200ex.chg, data = df)

result.lmchg

summary(result.lmchg)
