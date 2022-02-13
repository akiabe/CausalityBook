## 数値例で理解する最小二乗法

# サンプルデータを設定
y1 <- c(40, 20, 50, 10)
x1 <- c(5, 1, 3, 2)
plot(x1, y1)

# 予測値の計算
yhat1 <- 11.143 + 6.857 * x1
e1 <- y1 - yhat1

yhat2 <- 10.909 * x1
e2 <- y1 - yhat2

sum(e1)
sum(e2)

e1b <-e1^2
e2b <-e2^2
sum(e1b)
sum(e2b)

# 種明かし：最小二乗法による切片と傾きの公式
xbar <- mean(x1)
ybar <- mean(y1)
hensax <- x1 - xbar
hensay <- y1 - ybar
hensaxy <- hensax * hensay
num <- sum(hensaxy)
hensax2 <- hensax^2
denom <- sum(hensax2)
b1 <- num/denom
b0 <- ybar - b1 * xbar

model1 <- lm(y1 ~ x1)
summary(model1)

## 条件付き期待値としての回帰モデル

y1 <- c(1, 2, 3, 4, 5, 6, 7, 8, 9)
x1 <- c(1, 1, 1, 2, 2, 2, 3, 3, 3)
mean(y1)

model2 <- lm(y1 ~ x1)
summary(model2)
-1 + 3 * 1
-1 + 3 * 2
-1 + 3 * 3

plot(x1, y1)
abline(model2)

(1 + 2 + 3)/3
(4 + 5 + 6)/3
(7 + 8 + 9)/3

# 最小二乗法による切片と傾きの公式（補遺）

x1 <- c(5, 1, 3, 2)
y1 <- c(40, 20, 50, 10)
model3 <- lm(y1 ~ x1)
bOLS <- summary(model3)$coefficient[2, 1]
ussOLS <- sum((resid(model2))^2)

b1 <- NULL
uss <- NULL
set.seed(1)
for (i in 1:10000) {
  a1 <- 11.14286
  b1[i] <- runif(1, -10, 25)
  yhat <- a1 + b1[i] * x1
  uss[i] <- sum((y1 - yhat)^2)
}
summary(uss)
ussOLS

plot(b1, uss, col = 8, cex = 0.1, pch = 20)
abline(v = bOLS, lty = 2)
abline(h = ussOLS)
