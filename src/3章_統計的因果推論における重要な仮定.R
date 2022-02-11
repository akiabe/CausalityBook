# データの読み込み
rm(list = ls())
data03 <- read.csv(file.choose())
attach(data03)
summary(data03)
mean(y3[t1==1]) - mean(y3[t1==0])
mean(y1t) - mean(y0t)

# 共分散分析
model1 <- lm(y3 ~ x1 + t1)
summary(model1)
confint(model1, level = 0.95)

# 散布図
plot(x1[t1==0], y3[t1==0], xlim = c(60, 100), ylim = c(60, 100), pch = 4)
par(new = TRUE)
plot(x1[t1==1], y3[t1==1], xlim = c(60, 100), ylim = c(60, 100), pch = 4)
b0 <- summary(model1)$coefficient[1]
b1 <- summary(model1)$coefficient[2]
b2 <- summary(model1)$coefficient[3]
abline(a = b0, b = b1)
abline(a = b0 + b2, b = b1)
