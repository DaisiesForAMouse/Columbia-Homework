library(haven)
library(ggplot2)

problem1 <- read.csv("problem1.csv")
X <- problem1$cigarettes
Y <- problem1$covid
ols <- lm(Y ~ X, data = problem1)
summ <- summary(ols)
beta_0 <- summ$coefficients[1]
beta_1 <- summ$coefficients[2]

message("a)")
print(problem1)
cat("b) Sample mean of X: ", mean(X), "\n")
cat("b) Sample mean of Y: ", mean(Y), "\n")
cat("b) Sample standard deviation of X: ", sqrt(var(X)), "\n")
cat("b) Sample standard deviation of Y: ", sqrt(var(Y)), "\n")
cat("c) Correlation coefficient between X, Y: ", cor(X, Y), "\n")
message("d) The OLS regression is given by the following:\n")
print(summ)
cat("d) Estimated intercept term beta_0: ", beta_0, "\n")
cat("d) Estimated slope coefficient beta_1: ", beta_1, "\n")
cat("e) The equation of the regression is then Y_hat =",
    beta_0, "+", beta_1, "X_hat\n")
cat("f) We have that the estimated Y_i are\n")
print(data.frame("country"=problem1$country,
                 "cigarettes"=problem1$cigarettes,
                 "predicted covid Y_i"=predict(ols)))
cat("g) The OLS residuals are\n")
print(data.frame("country"=problem1$country,
                 "cigarettes"=problem1$cigarettes,
                 "predicted residuals u_i"=ols$residuals))