library(rstanarm)
library(ggplot2)

df106 <- read.csv("/Users/amelia/Desktop/678hw1/beauty.csv", header=T)

fit106 <- stan_glm(eval~beauty, data = df106)
print(fit106)
ggplot(df106, aes(beauty, eval))+ 
         geom_point(show.legend = FALSE) + 
         geom_abline(
           intercept = coef(fit106)[1], 
           slope = coef(fit106)[2], 
           color = "blue")
# The coefficient 4.0 reflects the predicted course evaluation for teacher whose beauty is 0. 
# The coefficient 0.1 means: If we compare a teacher with beauty of 0 to teacher with beauty of 1 expect the teacher to have difference in course evaluation of 0.1 on average.

residual <- coef(fit106)[1] + coef(fit106)[2] * df106$beauty
df106_1 <- data.frame(fitted(fit106), residual)
colnames(df106_1)[1] <- 'fitted_values'
ggplot(df106_1, aes(fitted_values, residual)) +
  geom_point(show.legend = FALSE)

# 10.6(b)
fit106_1 <- stan_glm(eval ~ beauty + female, data = df106)
print(fit106_1)
# The intercept 4.1 means: If a male teacher whose beauty is 0, then we would predict his course evaluation to be 4.1.
# The coefficient 0.1 means: Comparing teachers of the same gender, but whose beauty differs by 1 point, we would expect to see a difference of 0.1 in the course evaluation.
# The coefficient -0.2 means: Comparing teachers with the same value of beauty, but differ in gender, the model predicts an expected difference of 0.2.

fit106_2 <- stan_glm(eval ~ beauty + female + beauty:female, data = df106)
print(fit106_2)
# The intercept 4.1 represents the predicted course evaluation scores for male teachers whose beauty score is 0.
# The coefficient of beauty 0.2 can be thought of as the comparison of mean course evaluation scores across male teachers whose beauty differs by 1 point.
# The coefficient of female -0.2 can be conceived as the difference between the predicted course evaluation scores for male teachers who have beauty of 0, and female teachers who have beauty of 0.
# The coefficient on the interaction term -0.1 represents the difference in the slope for beauty, comparing male teachers and female teachers.
