# Zad 1.
# a)

x10 <- rnorm(10, 0, 1)
x50 <- rnorm(50, 0, 1)
x100 <- rnorm(100, 0, 1)
x500 <- rnorm(500, 0, 1)

qqnorm(x500)
qqline(x500)

# b)

x10 <- rgamma(10, shape = 2, scale = 2)

qqnorm(x10)
qqline(x10)

x500 <- rgamma(500, shape = 2, scale = 2)
qqnorm(x500)
qqline(x500)

# c)

x10 <- rcauchy(10)

qqnorm(x10)
qqline(x10)

x500 <- rcauchy(500)

qqnorm(x500)
qqline(x500)

# Zad 2.
# a)
A = matrix(c(-1, 4, -2, 5), ncol = 2, byrow = T)
A  

P <- eigen(A)$vectors
D <- diag(eigen(A)$values)

# b)

P %*% D %*% solve(P)

# c)

plot(NULL, xlim = c(-2, 2), ylim = c(-2, 2))
arrows(0, 0, P[1, 1], P[2, 1])  
arrows(0, 0, P[1, 2], P[2, 2])  
text(P[1, 1], P[2, 1], "x", pos = 1)
text(P[1, 2], P[2, 2], "y", pos = 1)

# d)

B <- matrix(c(2, 1, 1, 2), byrow = T, ncol = 2)
B

eigen(B)
solve(eigen(B)$vectors)

# e)

AA <- t(A) %*% A
eigAA <- eigen(AA)
PAA <- eigAA$vectors
DAA <- diag(eigAA$values)

diag(svd(A)$d^2)
DAA

PAA
svd(A)$v

# Zad 3
# a)
M <- as.matrix(read.csv("zebra.csv"))
image(M, asp=TRUE, col=c("white", "black"), xaxt = "n", yaxt = "n")

Msvd <- svd(M)

U <- Msvd$u
V <- Msvd$v
L <- diag(Msvd$d)

# b)
n <- nrow(M)

M50 <- U %*% diag(c(svd(M)$d[1:(n / 2)], rep(0, n / 2))) %*% t(V)
M10 <- U %*% diag(c(svd(M)$d[1:(n / 10)], rep(0, n - n / 10))) %*% t(V)
M4 <- U %*% diag(c(svd(M)$d[1:(n / 25)], rep(0, n - n / 25))) %*% t(V)
M2 <- U %*% diag(c(svd(M)$d[1:(n / 50)], rep(0, n - n / 50))) %*% t(V)

# c)

par(mfrow = c(2, 2))

image(M50, asp=TRUE, col=c("white", "black"), xaxt = "n", yaxt = "n")
image(M10, asp=TRUE, col=c("white", "black"), xaxt = "n", yaxt = "n")
image(M4, asp=TRUE, col=c("white", "black"), xaxt = "n", yaxt = "n")
image(M2, asp=TRUE, col=c("white", "black"), xaxt = "n", yaxt = "n")

# d)

par(mfrow = c(1, 1))

sing <- Msvd$d

plot(sing)
abline(v = c(n/2, n/10, n/25, n/50))
