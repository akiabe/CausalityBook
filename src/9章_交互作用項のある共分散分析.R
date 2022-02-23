data09 <- read.csv(file.choose())
attach(data09)
summary(data09)

mean(y1t) - mean(y0t)
model1 <- lm(y3 ~ x1 + t1)
summary(model1)

## 交互作用項のある共分散分析
x1t <- x1 * t1
model2 <- lm(y3 ~ x1 + t1 + x1t)
summary(model2)

# 平均処置効果の計算
b2 <- summary(model2)$coefficients[3, 1]
b3 <- summary(model2)$coefficients[4, 1]
b2 + b3 * mean(x1)

# 標準誤差の計算
vcov(model2)
v2 <- diag(vcov(model2))[3]
v3 <- diag(vcov(model2))[4]
x1bar <- mean(x1)
cov23 <- vcov(model2)[3, 4]
vartau <- v2 + x1bar^2 * v3 + x1bar * 2 * cov23
setau <- sqrt(vartau)
setau

# 簡便な分析方法
x1t2 <- (x1 - mean(x1)) * (t1 - mean(t1))
model3 <- lm(y3 ~ x1 + t1 + x1t2)
summary(model3)
