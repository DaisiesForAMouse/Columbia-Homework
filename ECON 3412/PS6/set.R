library(haven)
library(sandwich)
library(ggplot2)
library(clubSandwich)
library(lmtest)
library(car)
library(plm)
library(sampleSelection)
library(pscl)
library(margins)

## robust_lm <- function(formula, data, conditions) {
##   regression <- lm(formula, data = data)
##   robust_coef <- coeftest(regression, vcov = vcovHC(regression, "HC1"))

##   if (missing(conditions)) {
##     return (list(regression, robust_coef))
##   }

##   f_test = linearHypothesis(regression, conditions, vcov = vcovHC(regression, "HC1"))

##   return (list(regression, robust_coef, f_test))
## }

mroz <- read_dta("./mroz.dta")

mroz.lm <- lm(inlf ~ nwifeinc + educ + exper + expersq +
                age + kidslt6 + kidsge6, data = mroz)
coeftest(mroz.lm, vcovCL)
mroz.probit <- probit(inlf ~ nwifeinc + educ + exper + expersq +
                        age + kidslt6 + kidsge6, data = mroz)
coeftest(mroz.probit, vcovCL)
mroz.logit <- glm(inlf ~ nwifeinc + educ + exper + expersq +
                    age + kidslt6 + kidsge6, data = mroz,
                  family = binomial(link = "logit"))
coeftest(mroz.logit, vcovCL)

nrow(mroz[(predict(mroz.lm) >= 0.5) == mroz$inlf,])
nrow(mroz[(pnorm(predict(mroz.logit)) >= 0.5) == mroz$inlf,])
nrow(mroz[(((1 + exp(predict(mroz.logit)) ^ -1)^-1) >= 0.5) == mroz$inlf,])

linearHypothesis(mroz.lm, c("exper = 0", "expersq = 0"), vcov = vcovCL)
linearHypothesis(mroz.probit, c("exper = 0", "expersq = 0"), vcov = vcovCL)

vote <- read_dta("./vote1.dta")
vote$winA <- vote$voteA > 50
vote$dSpend <- vote$expendA - vote$expendB

vote.lm <- lm(winA ~ expendB + dSpend + prtystrA + democA, data = vote)
coeftest(vote.lm, vcovCL)
vote.probit <- probit(winA ~ expendB + dSpend + prtystrA + democA, data = vote)
coeftest(vote.probit, vcovCL)

ggplot(data = vote) + geom_point(aes(x = predict(vote.lm), y = winA))
nrow(vote[(predict(vote.lm) > 0.5) == vote$winA,])

vote.probit <- probit(winA ~ expendB + dSpend + prtystrA + democA, data = vote)
coeftest(vote.probit, vcovCL)

ggplot(data = vote) + geom_point(aes(x = pnorm(predict(vote.probit)), y = winA))
nrow(vote[(pnorm(predict(vote.probit)) > 0.5) == vote$winA,])


mean(0.006178 * 1 / sqrt(2 * pi) * exp(-(-1.686795 + -0.001898 * vote$expendB + 0.029483 * vote$prtystrA + 0.843552 * vote$democA + 0.006178 * -250)^2 / 2))
sd(0.006178 * 1 / sqrt(2 * pi) * exp(-(-1.686795 + -0.001898 * vote$expendB + 0.029483 * vote$prtystrA + 0.843552 * vote$democA + 0.006178 * -250)^2 / 2))
mean(0.006178 * 1 / sqrt(2 * pi) * exp(-(-1.686795 + -0.001898 * vote$expendB + 0.029483 * vote$prtystrA + 0.843552 * vote$democA + 0.006178 * 0)^2 / 2))
mean(0.006178 * 1 / sqrt(2 * pi) * exp(-(-1.686795 + -0.001898 * vote$expendB + 0.029483 * vote$prtystrA + 0.843552 * vote$democA + 0.006178 * 250)^2 / 2))