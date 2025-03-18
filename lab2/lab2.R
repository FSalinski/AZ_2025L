# 2.1

x = seq(0, 10, 0.1)

eps = rnorm(length(x), 0, 3)

y = x + eps

plot(x, y)

# a)

corr <- sum((x - mean(x)) * (y - mean(y))) / (sqrt(sum((x - mean(x))^2)) * sqrt(sum((y - mean(y))^2)))
corr

cor(x, y)

# b)

beta_1 = sum((x - mean(x)) * (y - mean(y))) / sum((x - mean(x))^2)
beta_0 = mean(y) - beta_1 * mean(x)
beta_1
beta_0

model <- lm(y ~ x)

model$coefficients
model$residuals

# c)

plot(x, y)
abline(model, col = "red")

# d)
# std = 0.5
x = seq(0, 10, 0.1)
eps = rnorm(length(x), 0, 0.5)
y = x + eps

cor(x, y)

model <- lm(y ~ x)
model$coef

plot(x, y)
abline(model, col = "red")

# std = 5
x = seq(0, 10, 0.1)
eps = rnorm(length(x), 0, 5)
y = x + eps

cor(x, y)

model <- lm(y ~ x)
model$coef

plot(x, y)
abline(model, col = "red")

# 2.4
# a) b)
n = 100

x1 = rnorm(n, 0, 1)
x2 = rnorm(n, 0, 1)
x3 = rnorm(n, 0, 1)
eps = rnorm(n, 0, 1)

y = 0.5 + x1 + 0.5 * x2 + 0.75 * x3 + eps

X = cbind(1, x1, x2, x3)
X
y

# c)

QR <- qr(X)

Q <- qr.Q(QR)
R <- qr.R(QR)

t(Q) %*% Q

# d)

# e)

solve(R, t(Q) %*% y)

# f)

method1 <- numeric(1000)
method2 <- numeric(1000)
method3 <- numeric(1000)

for (i in 1:1000){
  start_time <- Sys.time()
  solve(t(X) %*% X, t(X) %*% y)
  stop_time <- Sys.time()
  method1[i] <- stop_time - start_time
  
  start_time <- Sys.time()
  solve(t(X) %*% X) %*% (t(X) %*% y)
  stop_time <- Sys.time()
  method2[i] <- stop_time - start_time
  
  start_time <- Sys.time()
  solve(R, t(Q) %*% y)
  stop_time <- Sys.time()
  method3[i] <- stop_time - start_time
}

mean(method1)
mean(method2)
mean(method3)


# 2.2
library(MASS)
hills

# a)
par(mfrow = c(1, 2))
plot(hills$dist, hills$time)
plot(hills$climb, hills$time)

cor(hills$time, hills$dist)
cor(hills$time, hills$climb)

# b)
modeldist <- lm(time ~ dist, data = hills)
modelclimb <- lm(time ~ climb, data = hills)

par(mfrow = c(1, 2))
plot(hills$dist, hills$time)
abline(modeldist, col = "red")

plot(hills$climb, hills$time)
abline(modelclimb, col = "red")

summary(modeldist)

# b)

SST <- sum((hills$time - mean(hills$time))**2)
SSR <- sum((modeldist$fitted.values - mean(hills$time))**2)
SSE <- sum((modeldist$fitted.values - hills$time)**2)

SST
SSR
SSE

R2 <- SSR / SST
R2

SST <- sum((hills$time - mean(hills$time))**2)
SSR <- sum((modelclimb$fitted.values - mean(hills$time))**2)
SSE <- sum((modelclimb$fitted.values - hills$time)**2)

R2 <- SSR / SST
R2

summary(modeldist)$r.sq

cor(hills$time, hills$dist) ** 2

# c)
time15 <- modeldist$coef %*% c(1, 15)
time15

predict(modeldist, data.frame(dist = 15))

predict(modeldist, data.frame(dist = c(12, 14, 17, 20)))

# 2.3

ans <- read.table("anscombe_quartet.txt", header = T)

ans

par(mfrow = c(2, 2))
plot(ans$X1, ans$Y1)
plot(ans$X2, ans$Y2)
plot(ans$X3, ans$Y3)
plot(ans$X4, ans$Y4)

# a)

m1 <- lm(Y1 ~ X1, data = ans)
m2 <- lm(Y2 ~ X2, data = ans)
m3 <- lm(Y3 ~ X3, data = ans)
m4 <- lm(Y4 ~ X4, data = ans)

# b)

summary(m1)$r.sq
summary(m2)$r.sq
summary(m3)$r.sq
summary(m4)$r.sq

cor(ans$Y1, ans$X1)
cor(ans$Y2, ans$X2)
cor(ans$Y3, ans$X3)
cor(ans$Y4, ans$X4)

# c)

par(mfrow = c(2, 2))
plot(ans$X1, ans$Y1)
abline(m1)
plot(ans$X2, ans$Y2)
abline(m2)
plot(ans$X3, ans$Y3)
abline(m3)
plot(ans$X4, ans$Y4)
abline(m4)

