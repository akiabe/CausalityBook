## 標準誤差 ##

# 母数の集計
N1 <- 4
n1 <- 2
x1 <- c(165, 166, 171, 180)
mu <- mean(x1)
hensa <- x1 - mu
hensa2 <- hensa^2
sigma2 <- sum(hensa2)/N1
sigma <- sqrt(sigma2)

# 標本サイズ2のすべての標本を抽出
xs <- combn(x1, n1)
xbars <- apply(xs, 2, mean)
mean(xbars)
hensab <- xbars - mu
hensa2b <- hensab^2
sigma2b <- sum(hensa2b)/length(xbars)
sigmab <- sqrt(sigma2b)
sigmab

# 標準誤差の計算
se0 <- sigma/sqrt(n1)
correct <- sqrt((N1 - n1)/(N1 - 1))
se1 <- se0 * correct

## 信頼区間 ##

# 信頼区間の計算
qt(0.025, 49, lower.tail = FALSE) # 両側検定、有意水準5%、標本サイズ50

# 信頼区間による対応のある場合の2標本t検定
data04 <- read.csv(file.choose())
attach(data04)
summary(data04)

n1 <- nrow(data04)
diff <- y1t - y0t
m1 <- mean(diff)
s1 <- sd(diff)
talpha <- qt(0.025, n1-1, lower.tail = FALSE)
m1 + talpha * s1/sqrt(n1)
m1 - talpha * s1/sqrt(n1)

t.test(diff)

# 信頼区間による対応のない場合の2標本t検定（Welchの検定）
y0obs <- na.omit(y0)
y1obs <- na.omit(y1)
n0 <- length(y0obs)
n1 <- length(y1obs)
s0 <- sd(y0obs)
s1 <- sd(y1obs)
num <- (s1^2/n1 + s0^2/n0)^2
denom <- ((s1^2/n1)^2)/(n1-1) + ((s0^2/n0)^2)/(n0-1)
df1 <- num/denom
xbar <- mean(y1obs) - mean(y0obs)
se1 <- sqrt((s0^2/n0) + (s1^2/n1))
talpha <- qt(0.025, df1, lower.tail = FALSE)
xbar + talpha * se1
xbar - talpha * se1

t.test(y1obs, y0obs, var.equal = FALSE)
