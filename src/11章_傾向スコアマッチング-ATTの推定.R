# データ
rm(list = ls())
data11 <- read.csv(file.choose())
attach(data11)
summary(data11)

# 推定対象の真値
mean(y1t[t1==1]) - mean(y0t[t1==1])　# ATT
mean(y1t) - mean(y0t)                # ATE

# 傾向スコアマッチングによるATTの推定
library(MatchIt)
m.out1 <- matchit(t1 ~ x1 + x2 + x3 + x4 + x5 + x6,
                  data=data11, replace=TRUE, distance="glm", weights=weights)
m.data1 <- match.data(m.out1)
model1 <- lm(y3 ~ t1, data=m.data1, weights=weights)
model2 <- lm(y3 ~ t1 + x1 + x2 + x3 + x4 + x5 + x6, data=m.data1, weights=weights)

# クラスターに頑健な標準誤差
library(lmtest); library(sandwich)
coeftest(model2, vcov.=vcovCL, cluster=~weights)
coefci(model2, level=0.95, vcov.=vcovCL, cluster=~weights)

# バランシングの評価：サマリー
summary(m.out1)

# ラブプロットによるバランシングの評価1
diffa <- abs(summary(m.out1)$sum.all[, 3])
diffb <- abs(summary(m.out1)$sum.matched[, 3])
diff1 <- rev(diffa)
diff2 <- rev(diffb)
maxx <- max(diff1, diff2)
labels0 <- rownames(summary(m.out1)$sum.all)
labels1 <- rev(labels0)
dotchart(diff1, xlim=c(0, maxx), labels=c(labels1))
abline(v=0.00, col=8)
abline(v=0.10, col=8)
abline(v=0.05, lty=2, col=8)
par(new=TRUE)
dotchart(diff2, xlim=c(0, maxx), labels=c(labels1),
         pch=16, xlab="Absolute Standardizeds Mean Difference")

# ラブプロットによるバランシングの評価2
plot(summary(m.out1), position=NULL)

