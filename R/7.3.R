library("rstanarm")

# 7.3 (a)
n <- 100
a <- 2
b <- -3
c <- 1

set.seed(1)
x <- runif(n, 0, 50)
error <- rnorm(n, 0, 3)
y <- a + b*x + c*x^2 + error

fake <- data.frame(x, y)
fit <- stan_glm(y ~ x + I(x^2), data = fake)
print(fit)
a_hat <- coef(fit)[1]
b_hat <- coef(fit)[2]
c_hat <- coef(fit)[3]
#The fitted model is: y = 1.39 - 2.97x + x^2

# 7.3 (b)
plot(fake$x, fake$y, main="Data and fitted regression line", bty="l", pch=20,
     xlab = "x", ylab = "y")
curve(a_hat + b_hat*x + c_hat*x^2, 0, 50, col="blue", add=T)

# 7.2(c)
x_bar <- mean(fake$x)
text(x_bar, a_hat + b_hat*x_bar + c_hat*x_bar^2, expression(paste("y =", " 1.39 - 2.97x + ", x^2)), adj=0)











