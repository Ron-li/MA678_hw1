library("rstanarm")

# 8.8(a)
n <- 100
a <- 2
b <- 3
sigma <- 5

set.seed(1)
x <- runif(n, 0, 20)
error <- rnorm(n, 0, sigma)
y = a + b*x + error
fake <- data.frame(x, y)
fit1 <- lm(y~x, data = fake)
fit2 <- stan_glm(y~x, data = fake)
a_hat1 <- coef(fit1)[1]
b_hat1 <- coef(fit1)[2]
a_hat2 <- coef(fit2)[1]
b_hat2 <- coef(fit2)[2]
print(fit1)
print(fit2)

# 8.8(b)
plot(fake$x, fake$y, main="Data and 2 fitted regression lines", bty="l", pch=20,
     xlab = "x", ylab = "y")
abline(a_hat1, b_hat1, col = "blue")
abline(a_hat2, b_hat2, col = "red")
x_bar <- mean(fake$x)
text(x_bar, a_hat1 + b_hat1*x_bar-5, paste("y =", round(a_hat1, 2), "+", round(b_hat1, 2), "x"), adj=0, col = "blue")
text(x_bar-4, a_hat2 + b_hat2*x_bar+10, paste("y =", round(a_hat2, 2), "+", round(b_hat2, 2), "x"), adj=0, col = "red")
# y = 1.1+3.08x  & y = 1.12+3.08x

# 8.8(c)
#model: y = 1 + 5x + error, x~unif(2,7), error ~ N(0, 3), n = 50
set.seed(1)
x <- runif(5, 18, 20)
error <- rnorm(5, 0, 3)
y = 1 + 5*x + error
fake <- data.frame(x, y)
fit1 <- lm(y~x, data = fake)
fit2 <- stan_glm(y~x, data = fake)
a_hat1 <- coef(fit1)[1]
b_hat1 <- coef(fit1)[2]
a_hat2 <- coef(fit2)[1]
b_hat2 <- coef(fit2)[2]
print(fit1)
print(fit2)
plot(fake$x, fake$y, main="Data and 2 fitted regression lines", bty="l", pch=20,
     xlab = "x", ylab = "y")
abline(a_hat1, b_hat1, col = "blue")
abline(a_hat2, b_hat2, col = "red")
x_bar <- mean(fake$x)
text(x_bar, a_hat1 + b_hat1*x_bar+0.7, paste("y =", round(a_hat1, 2), "+", round(b_hat1, 2), "x"), adj=0, col = "blue")
text(x_bar, a_hat2 + b_hat2*x_bar-0.2, paste("y =", round(a_hat2, 2), "+", round(b_hat2, 2), "x"), adj=0, col = "red")
# y = 69.49+1.35x  & y = 69.75+1.32x
