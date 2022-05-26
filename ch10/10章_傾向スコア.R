## 10.4 傾向スコアのモデル化

# データの生成
rm(list = ls())
set.seed(1) # シード値
n1 <- 1000 # 標本サイズ
b0 <- 0.5
b1 <- 1.1

# 区間[-10, 10]の一様分布に従う乱数としてx1を生成
x1 <- runif(n1, -10, 10)

# ロジスティック分布に従う乱数としてe1を生成
e1 <- rlogis(n1, location=0, scale=1)
tstar <- b0 + b1*x1 + e1

t1 <- NULL
# tstarが0より大きければ1、0以下であれば0に振り分け 
t1[tstar>0] <- 1   
t1[tstar<=0] <- 0

# データの図示
plot(x1, tstar, col=8)
model1 <- lm(tstar ~ x1)
abline(model1, lwd=2)
plot(x1, t1, col=8)
lines(lowess(x1, t1), lwd=2)

# ロジスティック曲線の図示
tt <- plogis(x1) 
plot(x1, tt)

# 真の傾向スコアの算出
num <- exp(b0 + b1*x1)
denom <- 1 + exp(b0 + b1*x1)
p1 <- num/denom

# 傾向スコアのモデル化
model2 <- glm(t1 ~ x1, family=binomial(link="logit"))
summary(model2)
p2 <- model2$fitted.values  # モデルの予測値を取り出す

# 傾向スコアの比較
plot(p1, p2)
cor(p1, p2)   # ほぼ完全にp1とp2は一致

## 10.5 傾向スコアのモデル化

# データの読込み
rm(list = ls())
data <- read.csv("data03.csv")
attach(data)
summary(data)

# 傾向スコアのモデル化
psmodel <- glm(t1 ~ x1, family=binomial(link="logit"))
ps3 <- round(psmodel$fitted.values, 4)

# 傾向スコアの真値を設定
ps4 <- c(rep(0.8, 5), rep(0.6, 5), rep(0.4, 5), rep(0.2, 5))

# 結果のまとめ
df2 <- data.frame(x1, y3, t1, ps3, ps4)
df2   # 傾向スコアはロジスティック回帰モデルから適切に予測できている

# パッケージを使用して、傾向スコアをモデル化する場合
library(MatchIt)
m.out <- matchit(t1 ~ x1, data=data)
ps5 <- m.out$model$fitted.values
summary(ps3)
summary(ps5)
cor(ps3, ps5)  # glm関数で計算した値と完全に一致
