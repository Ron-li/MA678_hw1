library(rstanarm)
library(ggplot2)

# 10.2(a)
# The equstion of the estimated regression line of y on x for the treatment group and the control group is y = 1.2 + 1.6x + 2.7z + 0.7xz
# The equstion of the estimated regression line of y on x for the control group is y = 1.2 + 1.6x

# 10.2(b)
set.seed(1)
b_hat <- c(1.2, 1.6, 2.7, 0.7)
x <- runif(100, 0, 10)
error <- rnorm(100, 0, 0.5)
z <- round(runif(100, 0, 1))
y <- b_hat[1] + b_hat[2]*x + b_hat[3]*z + b_hat[4]*x*z + error
df <- data.frame(x, z, y)

x_bar <- mean(df$x)
ggplot(df, aes(x, y)) +
  geom_point(aes(color = ifelse(z==0, "blue", "red")), pch=ifelse(z==0, 1, 20), show.legend = FALSE) +
  geom_abline(
    intercept = c(b_hat[1], b_hat[1] + b_hat[3]),
    slope = c(b_hat[2], b_hat[2] + b_hat[4]),
    color = c("red", "blue")) + 
  geom_text(aes(x=x_bar,y=b_hat[1] + b_hat[2]*x_bar),label="y = 1.2+1.6x",
            size=5,col = "red") + 
  geom_text(aes(x=x_bar,y=b_hat[1] + b_hat[3] + (b_hat[2]+b_hat[4])*x_bar),label="y = 3.9+2.3x",
            size=5,col = "blue") + 
  labs(title = "Date and two regression lines")