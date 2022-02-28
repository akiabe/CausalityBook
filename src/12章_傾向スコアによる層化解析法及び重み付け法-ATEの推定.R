## 傾向スコア層化解析：ATEの推定
# データの読み込み
rm(list = ls())
data11 <- read.csv(file.choose())
mean(data11$y1t) - mean(data11$y0t)　# ATE

# 傾向スコアのモデル化
library(MatchIt)
sub <- 5
m.out2 <- matchit(t1 ~ x1 + x2 + x3 + x4 + x5 + x6,
                  data=data11, method="subclass", subclass=sub, estimand="ATE", min.n=2)
m.data2 <- match.data(m.out2)

# 傾向スコア層化解析によるATEの推定
library(lmtest); library(sandwich)
psp <- NULL; psvar <- NULL; nps <- NULL; robustvar <- NULL
for (j in 1:sub) {
  dataps <- m.data2[m.data2$subclass==j, ]
  model4 <- lm(y3 ~ t1 + x1 + x2 + x3 + x4 + x5 + x6, data=dataps)
  psp[j] <- summary(model4)$coefficients[2, 1]
  psvar[j] <- summary(model4)$coefficients[2, 2]^2
  nps[j] <- nrow(dataps)
  robustvar[j] <- coeftest(model4, vcov.=vcovCL, cluster=~weights)[2, 2]
}

# 出力結果の表示
n1 <- nrow(data11)
tauhat <- sum((nps/n1) * psp)
vartau <- sum((nps/n1)^2 * psvar)
setau <- sqrt(vartau)
robustvartau <- sum((nps/n1)^2 * robustvar)
robustsetau <- sqrt(robustvartau)

# 95%信頼区間
tstar <- qt(0.975, n1 - 8)
tauhat + tstar * robustsetau
tauhat - tstar * robustsetau

# バランシングの評価：サマリー
summary(m.out2)

# バランシングの評価：ラブプロット
diffa <- abs(summary(m.out2)$sum.all[, 3])
diffb <- abs(summary(m.out2)$sum.across[, 3])
diff1 <- rev(diffa)
diff2 <- rev(diffb)
maxx <- max(diff1, diff2)
labels0 <- rownames(summary(m.out2)$sum.all)
labels1 <- rev(labels0)
dotchart(diff1, xlim=c(0, maxx), labels=c(labels1))
abline(v=0.00, col=8)
abline(v=0.10, col=8)
abline(v=0.05, lty=2, col=8)
par(new=TRUE)
dotchart(diff2, xlim=c(0, maxx), labels=c(labels1),
         pch=16, xlab="Absolute Standardizeds Mean Difference")

## 傾向スコアの重み付け法：ATEの推定
# 傾向スコアのモデル化と重みの計算
model1 <- glm(t1 ~ x1 + x2 + x3 + x4 + x5 + x6, 
              data=data11, family=binomial(link="logit"))
ps1 <- model1$fitted.values
if1 <- data11$t1==1
if0 <- data11$t1==0
weights1 <- NULL
weights1[if1] <- 1/ps1[if1]
weights1[if0] <- 1/(1-ps1[if0])

# 傾向スコアの重み付け法による解析
model2 <- lm(y3 ~ t1, data=data11, weights=weights1)
model3 <- lm(y3 ~ t1 + x1 + x2 + x3 + x4 + x5 + x6, data=data11, weights=weights1)
summary(model2)
summary(model3)

# 標準誤差
library(lmtest); library(sandwich)
coeftest(model3, vcov.=vcovCL, cluster=weights1)
coefci(model3, level=0.95, vcov.=vcovCL, cluster=weights1)
