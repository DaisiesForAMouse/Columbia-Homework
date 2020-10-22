library(haven)
library(sandwich)
library(lmtest)

teach_data <- read_dta("TeachingRatings.dta")

eval_1 <- lm(course_eval ~ beauty + age, data = teach_data)
cat("R^2: ", summary(eval_1)$r.squared, "\n")
cat("Adjusted R^2: ", summary(eval_1)$adj.r.squared, "\n")
cat("Regression RMSE: ", sqrt(mean(eval_1$residuals^2)), "\n")
cat("n: ", length(eval_1$residuals), "\n")
print(coeftest(eval_1, vcov = vcovHC(eval_1, "HC1")))

eval_2 <- lm(course_eval ~ beauty + age + minority, data = teach_data)
cat("R^2: ", summary(eval_2)$r.squared, "\n")
cat("Adjusted R^2: ", summary(eval_2)$adj.r.squared, "\n")
cat("Regression RMSE: ", sqrt(mean(eval_2$residuals^2)), "\n")
cat("n: ", length(eval_2$residuals), "\n")
print(coeftest(eval_2, vcov = vcovHC(eval_2, "HC1")))

eval_3 <- lm(course_eval ~ beauty + age +
               minority + nnenglish,
             data = teach_data)
cat("R^2: ", summary(eval_3)$r.squared, "\n")
cat("Adjusted R^2: ", summary(eval_3)$adj.r.squared, "\n")
cat("Regression RMSE: ", sqrt(mean(eval_3$residuals^2)), "\n")
cat("n: ", length(eval_3$residuals), "\n")
print(coeftest(eval_3, vcov = vcovHC(eval_3, "HC1")))

eval_4 <- lm(course_eval ~ beauty + age + minority + nnenglish +
               intro + onecredit + female,
             data = teach_data)
cat("R^2: ", summary(eval_4)$r.squared, "\n")
cat("Adjusted R^2: ", summary(eval_4)$adj.r.squared, "\n")
cat("Regression RMSE: ", sqrt(mean(eval_4$residuals^2)), "\n")
cat("n: ", length(eval_4$residuals), "\n")
print(coeftest(eval_4, vcov = vcovHC(eval_4, "HC1")))

print(waldtest(eval_1, vcov = vcovHC(eval_1, type = "HC1")))

print(waldtest(eval_2, lm(course_eval ~ minority, data = teach_data),
               vcov = vcovHC(eval_2, type = "HC1")))
print(waldtest(eval_2, vcov = vcovHC(eval_2, type = "HC1")))

print(waldtest(eval_3,
               lm(course_eval ~ minority + nnenglish, data = teach_data),
               vcov = vcovHC(eval_3, type = "HC1")))
print(waldtest(eval_3,
               lm(course_eval ~ nnenglish, data = teach_data),
               vcov = vcovHC(eval_3, type = "HC1")))
print(waldtest(eval_3, vcov = vcovHC(eval_3, type = "HC1")))

print(waldtest(eval_4,
               lm(course_eval ~ minority + nnenglish + intro +
                    onecredit + female, data = teach_data),
               vcov = vcovHC(eval_4, type = "HC1")))
print(waldtest(eval_4, lm(course_eval ~ nnenglish + intro +
                            onecredit + female, data = teach_data),
               vcov = vcovHC(eval_4, type = "HC1")))
print(waldtest(eval_4, lm(course_eval ~ intro + onecredit +
                            female, data = teach_data),
               vcov = vcovHC(eval_4, type = "HC1")))
print(waldtest(eval_4, lm(course_eval ~ beauty + age + minority +
                            nnenglish + female, data = teach_data),
               vcov = vcovHC(eval_4, type = "HC1")))
print(waldtest(eval_4, lm(course_eval ~ beauty + nnenglish + intro +
                            onecredit + female, data = teach_data),
               vcov = vcovHC(eval_4, type = "HC1")))
print(waldtest(eval_4, lm(course_eval ~ beauty + minority + nnenglish +
                            onecredit + female, data = teach_data),
               vcov = vcovHC(eval_4, type = "HC1")))
