library(haven)
library(lmtest)
library(sandwich)
library(car)
library(ggplot2)

robust_lm <- function(formula, data, conditions) {
  regression <- lm(formula, data = data)
  robust_coef <- coeftest(regression, vcov = vcovHC(regression, "HC1"))

  if (missing(conditions)) {
    return (list(regression, robust_coef))
  }

  f_test = linearHypothesis(regression, conditions, vcov = vcovHC(regression, "HC1"))

  return (list(regression, robust_coef, f_test))
}

sleep_dta <- read_dta("sleep75.dta ")
robust_lm(sleep ~ totwrk + educ + age, sleep_dta, c("educ = 0", "age = 0"))
summary(robust_lm(sleep ~ totwrk + educ + age, sleep_dta, c("educ = 0", "age = 0"))[[1]])
robust_lm(sleep ~ totwrk, sleep_dta, c("educ = 0", "age = 0"))

college_dta <- read_dta("CollegeDistance_for Midterm.dta")
robust_lm(ln_ed ~ black + dist + blackxdist + momcoll + incomehi, college_dta)
robust_lm(ln_ed ~ black + dist + blackxdist + momcoll + incomehi, college_dta, c("black = 0", "dist = 0", "blackxdist = 0"))
robust_lm(ln_ed ~ black + dist + momcoll + incomehi + blackxincomehi, college_dta)
robust_lm(ln_ed ~ black + dist + momcoll + incomehi + blackxincomehi, college_dta, "blackxincomehi = 0")
robust_lm(ln_ed ~ black + dist + momcoll + incomehi + blackxincomehi, college_dta, "blackxincomehi = -black")


## Plotting example:
## ggplot(data=terror_dta) +
##   geom_point(data=terror_dta[terror_dta$higdppc == 1,], aes(x=lackpf, y=lnftmpop), col="blue") +
##   geom_point(data=terror_dta[terror_dta$higdppc == 0,], aes(x=lackpf, y=lnftmpop), col="red")  +
##   geom_abline(slope = -0.24825 + 0.6449, intercept=-0.717-2.838, col="blue") +
##   geom_abline(slope = -0.24825, intercept=-0.717, col="red")