# データの読み込み
rm(list = ls())
data02 <- read.csv(file.choose())
attach(data02)
head(data02, n = 5)
summary(data02)

# 個体処置効果
y1t - y0t
y3 - x1
y1 - y0

# 平均処置効果
mean(y1t) - mean(y0t)
mean(y3) - mean(x1)
m1 <- mean(y1, na.rm = TRUE)
m0 <- mean(y0, na.rm = TRUE)
m1 - m0

# 処置群の平均処置効果
mt1 <- mean(y1t[t1==1])
mt0 <- mean(y1t[t1==0])
mt1 - mt0

# 無作為割付けのよる分析
set.seed(1)
r0 <- runif(20, 0, 1)
r1 <- round(r0, 0)
y2 <- NULL
y2[r1==1] <- y1t[r1==1]
y2[r1==0] <- y1t[r1==0]
r1
y2

mr1 <- mean(y2[r1==1])
mr0 <- mean(y2[r1==0])
mr1
mr0
mr1 - mr0

# 2標本t検定
t.test(y2[r1==1], y2[r1==0], var.equal = FALSE)
