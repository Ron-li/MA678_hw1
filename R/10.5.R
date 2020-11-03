library(rstanarm)
library(ggplot2)

data_kid <- read.csv("/Users/amelia/Documents/MA678/kidiq/kidiq.csv", header = TRUE)
kidiq <- head(data_kid, 400)

# 10.5(a)
fit <- stan_glm(kid_score~mom_age, data = kidiq)
print(fit)
ggplot(kidiq, aes(mom_age, kid_score)) + 
  geom_point() + 
  geom_abline(
    intercept = coef(fit)[1], 
    slope = coef(fit)[2],
    col = "blue") + 
  geom_text(aes(x=23, y=92), label="y = 66.7 + 0.9x",
             size=5, col = "blue")
# The assumptions:(Gauss-Markov)
# (1)var(error) is the same; (2)cov(error) = 0
error <- fitted(fit) - kidiq$kid_score
df <- data.frame(kidiq, fitted(fit), error)
ggplot(df, aes(x = error)) + geom_density()
qqnorm(df$error)
# according to the image, the assumption is not fulfuilled
# The slope coefficient 0.7: if we compare a mother with age of 0 to mother with age of 1 expect the child to have difference in IQ of 0.7 on average.


a <- tapply(df$kid_score, df$mom_age, mean)
print(a)
# I recommend mothers should give birth at the age of 29. I assume the group size of every age is equal.

# 10.5(b)
fit1 <- stan_glm(kid_score~mom_age + mom_hs, data = kidiq)
print(fit1)
# The slope coefficient of 11.5: Comparing childern whose mothers have the same age, but who differed in whether they completed high school, the model predicts an expected difference of 11.5 in their test scores.
# The slope coefficient of 0.5: Comparing childern with the same value of mom_hs, but whose mothers differ by 1 year in age, we would expect to see a difference of 0.5 points in the child's test score.
# My recommendation remains unchanged.

# 10.5(c)
fit2 <- stan_glm(kid_score~mom_age + mom_hs + mom_age:mom_hs, data = kidiq)
print(fit2)
b_hat <- coef(fit2)
x_bar <- mean(kidiq$mom_age)
ggplot(kidiq, aes(mom_age, kid_score)) +
  geom_point(aes(color = ifelse(mom_hs==0, "blue", "red")), show.legend = F) +
  geom_abline(
    intercept = c(b_hat[1], b_hat[1] + b_hat[3]),
    slope = c(b_hat[2], b_hat[2] + b_hat[4]),
    color = c("red", "blue")) + 
  geom_text(aes(x=x_bar,y=b_hat[1] + b_hat[2]*x_bar),label="y = 107.93 - 1.45x",
            size=7,col = "red") + 
  geom_text(aes(x=x_bar,y=b_hat[1] + b_hat[3] + (b_hat[2]+b_hat[4])*x_bar),label="y = 67.23 + 0.98x",
            size=7,col = "blue")

# 10.5(d)
df2 <- kidiq[1:200,]
df3 <- kidiq[201:400,]
fit3 <- stan_glm(kid_score ~ mom_age + mom_hs + mom_age:mom_hs, data = df2)
print(fit3)
score_hat <- coef(fit3)[1] + coef(fit3)[2]*df3$mom_age + coef(fit3)[3]*df3$mom_hs + coef(fit3)[4]*df3$mom_age*df3$mom_hs
df4 <- data.frame(df3, score_hat)
ggplot(df4) +
  geom_point(aes(mom_age, kid_score), show.legend = FALSE, col = "blue") +
  geom_point(aes(mom_age, score_hat), show.legend = FALSE, col = "red")
#blue dots are the actual scores; red dots are the predicted scores




