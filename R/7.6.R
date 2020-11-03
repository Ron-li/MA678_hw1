library("rstanarm")

hibbs <- read.table("/Users/amelia/Desktop/678hw1/hibbs.dat", header=TRUE)

# 7.6(a)
x <- data.frame()
for (i in 1:16){
  
x[i, 1] <- ifelse(hibbs[i, 2] > 2, 1, 0)

}
newdata <- data.frame(hibbs$vote, x)
mean_diff <- tapply(newdata[, 1], newdata$V1, mean)
se <- sqrt(var(newdata$hibbs.vote[newdata$V1==1])/length(newdata$hibbs.vote[newdata$V1==1])+
           var(newdata$hibbs.vote[newdata$V1==0])/length(newdata$hibbs.vote[newdata$V1==1]))
diff <- mean_diff[2] - mean_diff[1]
df76 <- data.frame(diff, se)
row.names(df76) = "growth"
print(df76)

# 7.6(b)
fit <- stan_glm(hibbs.vote~V1, data = newdata)
print(fit)
