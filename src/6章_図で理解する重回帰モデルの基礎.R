# データの読み込み
data06 <- read.csv(file.choose())
attach(data06)
head(data06, n=10)
summary(data06)
n1 <- nrow(data06)

# 分散
hensa <- y1 - mean(y1)
hensa2 <- hensa^2
tss <- sum(hensa2)
tss/(n1 - 1)

var(y1)

# ESSとUSS
model1 <- lm(y1 ~ x1)
summary(model1)
confint(model1, level=0.95)
plot(x1, y1)

yhat1 <- -3.4217 + 2.7044*x1
yhat2 <- (yhat1 - mean(y1))^2
ess <- sum(yhat2)

predict(model1)

e1 <- y1 - yhat1
e2 <- e1^2
uss <- sum(e2)

resid(model1)

# 決定係数
1 - uss/tss
summary(model1)$r.squared

# 三変数の重回帰モデル
model1 <- lm(x1 ~ x2)
ex1 <- resid(model1)
model2 <- lm(y1 ~ ex1)
model3 <- lm(y1 ~ x1 + x2)
summary(model2)
summary(model3)
confint(model3, level=0.95)

# 共分散分析
data03 <- read.csv(file.choose())
attach(data03)
head(data03, n=10)

model1 <- lm(t1 ~ x1)
et1 <- resid(model1)
model2 <- lm(y3 ~ et1)
summary(model2)

