library("rstanarm")

# 7.2(a)
a <- 5
b <- 7
sigma <- 3
n = 100

set.seed(1)
x <- runif(n, 0, 50)
error <- rnorm(n, 0, sigma)
y <- a + b*x + error
fake <- data.frame(x, y)

fit <- stan_glm(y~x, data = fake)
print(fit)
a_hat <- coef(fit)[1]
b_hat <- coef(fit)[2]
# The fitted regression line is : y=4.48+7.02x

# 7.2 (b)
plot(fake$x, fake$y, main="Data and fitted regression line", bty="l", pch=20,
     xlab = "x", ylab = "y")
abline(a_hat, b_hat, col="blue")

# 7.2(c)
x_bar <- mean(fake$x)
text(x_bar, a_hat + b_hat*x_bar, paste("y =", round(a_hat, 2), "+", round(b_hat, 2), "* x"), adj=0)

