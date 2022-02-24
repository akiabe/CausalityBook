## 傾向スコアのモデル化
# データの生成
set.seed(1)
n1 <- 1000
b0 <- 0.5
b1 <- 1.1
x1 <- runif(n1, -10, 10)
e1 <- rlogis(n1, location=0, scale=1)
tstar <- b0 + b1*x1 + e1
t1 <- NULL
t1[tstar>0] <- 1
t1[tstar<=0] <- 0

# データの図示
plot(x1, tstar, col=8)
model1 <- lm(tstar ~ x1)
abline(model1, lwd=2)
plot(x1, t1, col=8)
lines(lowess(x1, t1), lwd=2)

# ロジスティック曲線
tt <- plogis(x1)
plot(x1, tt)

# 真の傾向スコアの計算
num <- exp(b0 + b1*x1)
denom <- 1 + exp(b0 + b1*x1)
p1 <- num/denom

# 傾向スコアのモデル化
model2 <- glm(t1 ~ x1, family=binomial(link="logit"))
summary(model2)
p2 <- model2$fitted.values

# 傾向スコアの比較
plot(p1, p2)
cor(p1, p2)

## 傾向スコアのモデル化の例
# データの読み込み
rm(list = ls())
data03 <- read.csv(file.choose())
attach(data03)
summary(data03)

# 傾向スコアのモデル化
psmodel <- glm(t1 ~ x1, family=binomial(link="logit"))
ps3 <- round(psmodel$fitted.values, 4)
ps4 <- c(rep(0.8, 5), rep(0.6, 5), rep(0.4, 5), rep(0.2, 5))
df2 <- data.frame(x1, y3, t1, ps3, ps4)

# MatchItによる傾向スコアのモデル化
library(MatchIt)
m.out <- matchit(t1 ~ x1, data=data03)
ps5 <- m.out$model$fitted.values
summary(ps3)
summary(ps5)
cor(ps3, ps5)
