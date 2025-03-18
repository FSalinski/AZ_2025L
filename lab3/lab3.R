# 3.1

data <- read.table('realest.txt', header = T)
head(data)

model <- lm(Price ~ ., data = data)
model

# a)

X <- as.matrix(cbind(1, data[,-1]))
X

Y <- as.matrix(data[,1])
Y

# b)

beta_MNK = solve(t(X) %*% X) %*% t(X) %*% Y

beta_MNK

model$coef

SST <- sum((Y - mean(Y))**2)
SSR <- sum((model$fitted.values - mean(Y))**2)
SSE <- sum((model$fitted.values - Y)**2)

R2 <- SSR / SST

R2

# c)

model$coef[2]

modelbedroom <- lm(Price ~ Bedroom, data = data)
modelbedroom

# d)

model$coef %*% c(1, 3, 1500, 8, 40, 1000, 2, 1, 0)

predict(model, data.frame(Bedroom = 3, Space = 1500, Room = 8, Lot = 40,
                          Tax = 1000, Condition = 0, Garage = 1, Bathroom = 2))

# e)
nrow(X) -> n
ncol(X) -> p

SSE = sum(model$residuals ** 2)

S2 = SSE / (n - p)
S2

summary(model)$sigma**2

# 3.2

# a) dowod na tablicy

# b)

X1 = rnorm(100)
X2 = rnorm(100)
X3 = rnorm(100)

eps = rnorm(100, 0, 10)

Y = 2 + 0.5 * X1 + X2 + 0.7 * X3 + eps
Y

df <- as.data.frame(cbind(X1, X2, X3, Y))
df

model <- lm(Y ~ ., data = df)

sum(model$fitted.values - Y) # zgadza sie
