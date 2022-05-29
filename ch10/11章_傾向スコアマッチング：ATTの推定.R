# 11.9 復元抽出の傾向スコアマッチング：ATTの推定

# データの読込み
rm(list = ls())
data <- read_csv(file.choose())
head(data)
attach(data)
summary(data)

# 傾向スコアマッチングによるATTの推定
library(MatchIt)

m.out1 <- matchit(t1 ~ x1 + x2 + x3 + x4 + x5 + x6,
                  # replaceをTRUEとして復元マッチングを指定
                  # distanceをglmtとしてロジスティック回帰を指定
                  # methodをnearestとして最近隣法を指定
                  data=data, replace=TRUE, distance="glm", method="nearest")

# マッチング後のデータを作成
m.data1 <- match.data(m.out1)

# マッチング後のデータからATTを推定
model1 <- lm(y3 ~ t1, data=m.data1, weights=weights)
summary(model1)

# マッチング後のデータから共分散分析を実施
model2 <- lm(y3 ~ t1 + x1 + x2 + x3 + x4 + x5 + x6, data=m.data1, weights=weights)
summary(model2)

# 11.10 標準誤差について

# クラスターに頑健な標準誤差
library(lmtest); library(sandwich)
coeftest(model2, vcov.=vcovCL, cluster=~weights)           # 推定値と標準誤差
coefci(model2, level=0.95, vcov.=vcovCL, cluster=~weights) # 95% 信頼区間

# 11.11 傾向スコアによるバランシングの評価

# バランシングの評価：サマリー
summary(m.out1)

# ラブプロットによるバランシングの評価（スクラッチ）
diffa <- abs(summary(m.out1)$sum.all[,3])
diffb <- abs(summary(m.out1)$sum.matched[,3])
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
         pch=16, xlab="Absolute Standardized Mean Difference")

# ラブプロットによるバランシングの評価
plot(summary(m.out1), position=NULL)
