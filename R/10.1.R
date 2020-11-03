library(rstanarm)
library(ggplot2)

# 10.1(a)
set.seed(1)
zi <- rbinom(100, 1, 0.5)
z <- data.frame(zi)
error <- rnorm(100, 0, 3)
x <- data.frame()
for (i in 1:100){

x[i, 1] <- rnorm(1, z[i, 1], 1)

}
y = 1 + 2*x - z - 2*x*z + error
fake <- data.frame(x, z, y)
colors <- ifelse(fake$zi == 0, "blue", "red")
pch1 <- ifelse(fake$zi == 0, 20, 1)
plot(fake$V1, fake$V1.1, xlab = "x", ylab = "y",
     main="Simulated data", col = colors, pch = pch1)
legend("bottomleft", inset=0.01,c("z=0", "z=1"), col=c("blue", "red"), lty = c(2, 4))
# z = 0,blue,filled dots ; z = 1,red,circles

# 10.1(b)
fit0 <- stan_glm(V1.1 ~ V1 + zi, data = fake)
print(fit0)
b_hat <- coef(fit0)
abline(b_hat[1], b_hat[2], col="blue")
abline(b_hat[1] + b_hat[3], b_hat[2], col="red")
x_bar <- mean(fake$V1)
text(x_bar-0.5, b_hat[1] + b_hat[2]*x_bar+1.5, 
     paste("y =", round(b_hat[1], 2), "+", round(b_hat[2], 2), "x"), 
     adj=0, col = "blue")
text(x_bar-0.5, b_hat[1] + b_hat[3] + b_hat[2]*x_bar-1, 
     paste("y =", round(b_hat[1] + b_hat[3], 2), "+", round(b_hat[2], 2), "x"), 
     adj=0, col = "red")
# z = 0,y = 0.81+1.26x ; z = 1, y = -1.14 + 1.26x

# 10.1(c)
fit1 <- stan_glm(V1.1 ~ V1 + zi + V1:zi, data = fake, refresh = 0)
print(fit1)
colors <- ifelse(fake$zi == 0, "blue", "red")
pch1 <- ifelse(fake$zi == 0, 20, 1)
plot(fake$V1, fake$V1.1, xlab = "x", ylab = "y", col = colors, pch = pch1)
b_hat <- coef(fit1)
abline(b_hat[1], b_hat[2], col="blue")
abline(b_hat[1] + b_hat[3], b_hat[2] + b_hat[4], col="red")
text(x_bar-0.5, b_hat[1] + b_hat[2]*x_bar+1.5, 
     paste("y =", round(b_hat[1], 2), "+", round(b_hat[2], 2), "x"), 
     adj=0, col = "blue")
text(x_bar-0.5, b_hat[1] + b_hat[3] + b_hat[2]*x_bar-1, 
     paste("y =", round(b_hat[1] + b_hat[3], 2), "+", round(b_hat[2] + b_hat[4], 2), "x"), 
     adj=0, col = "red")
# z = 0, y = 0.78 + 2.23x; z = 1, y =0.06 + 0.07x