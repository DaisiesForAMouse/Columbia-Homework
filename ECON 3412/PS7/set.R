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
library(ivreg)

ajr <- read_dta("./AJR2001.dta")

ajr.ols <- lm(loggdp ~ risk, data = ajr)
ajr.rfreg <- lm(risk ~ logmort0, data = ajr)
ajr.ivreg <- ivreg(loggdp ~ risk | logmort0, data = ajr)

coeftest(ajr.ols)
coeftest(ajr.rfreg)
coeftest(ajr.ivreg)
coeftest(ajr.ols, vcovCL)
coeftest(ajr.rfreg, vcovCL)
coeftest(ajr.ivreg, vcovCL)

lm(loggdp ~ logmort0, data = ajr)
ajr.olsnew <- lm(loggdp ~ risk + africa + latitude, data = ajr)
coeftest(ajr.olsnew, vcovCL)

ajr.ivregnew <- ivreg(loggdp ~ risk + africa + latitude |
                        logmort0 + africa + latitude, data = ajr)
coeftest(ajr.ivregnew, vcovCL)

ajr$mort0 <- exp(ajr$logmort0)
ajr.rfregexp <- lm(risk ~ mort0, data = ajr)
coeftest(ajr.rfregexp, vcovCL)
ajr.ivregexp <- ivreg(loggdp ~ risk | mort0, data = ajr)
coeftest(ajr.ivregexp, vcovCL)

ajr.rfregquad <- lm(risk ~ logmort0 + I(logmort0^2), data = ajr)
coeftest(ajr.rfregquad, vcovCL)
ajr.ivregquad <- ivreg(loggdp ~ risk | logmort0 + I(logmort0^2), data = ajr)
coeftest(ajr.ivregquad, vcovCL)

linearHypothesis(ajr.rfregquad, c("logmort0 = 0", "I(logmort0^2) = 0"),
                 vcov = vcovHC(ajr.rfregquad, "HC1"))

ajr.Jtest <- lm(ajr.ivregquad$residuals ~ logmort0 + I(logmort0^2), data = ajr)
linearHypothesis(ajr.Jtest, c("logmort0 = 0", "I(logmort0^2) = 0"),
                 vcov = vcovHC(ajr.Jtest, "HC1"))


bkh <- read_dta("./BKHousing.dta")
bkh$logBinCount <- log(bkh$BinCount)
bkh$Treatment <- bkh$UBound5K <= 175000
bkh$PostCut <- bkh$Month >= 584 & bkh$Month <= 599
bkh$HasCut <- bkh$Treatment & bkh$PostCut

bkh.MonthCut <- c()
bkh.MonthNoCut <- c()
bkh.Treatment <- c()
bkh.Control <- c()
for (month in unique(bkh$Month)) {
  if (length(bkh[bkh$Month == month & bkh$HasCut == 1,]$logBinCount) > 0) {
    cut <- mean(bkh[bkh$Month == month & bkh$HasCut == 1,]$logBinCount)
    bkh.MonthCut <- append(bkh.MonthCut, cut)
  }
  nocut <- mean(bkh[bkh$Month == month & bkh$HasCut == 0,]$logBinCount)
  bkh.MonthNoCut <- append(bkh.MonthNoCut, nocut)

  treat <- mean(bkh[bkh$Month == month & bkh$Treatment == 1,]$logBinCount)
  bkh.Treatment <- append(bkh.Treatment, treat)
  cont <- mean(bkh[bkh$Month == month & bkh$Treatment == 0,]$logBinCount)
  bkh.Control <- append(bkh.Control, cont)
}

ggplot() + geom_line(aes(x = unique(bkh$Month), y = bkh.MonthNoCut), color = "blue") +
  geom_point(aes(x = unique(bkh$Month), y = bkh.MonthNoCut), color = "blue") +
  geom_line(aes(x = unique(bkh$Month), y = bkh.Treatment), color="green") +
  geom_point(aes(x = unique(bkh$Month), y = bkh.Treatment), color = "green") +
  geom_line(aes(x = unique(bkh$Month), y = bkh.Control), color = "black") +
  geom_point(aes(x = unique(bkh$Month), y = bkh.Control), color = "black") +
  geom_line(aes(x = 584:599, y = bkh.MonthCut), color="red") +
  geom_point(aes(x = 584:599, y = bkh.MonthCut), color = "red")

bkh.dd <- plm(logBinCount ~ HasCut, data = bkh, index = c("UBound5K", "Month"),
              model = "within", effect = "twoways")
coef_test(bkh.dd, vcov = "CR1S", cluster = bkh$UBound5K)

bkh$Value <- bkh$UBound5K - 2500
bkh.ddint <- plm(logBinCount ~ HasCut + I(HasCut * Value), data = bkh, index = c("UBound5K", "Month"),
                 model = "within", effect = "twoways")
coef_test(bkh.ddint, vcov = "CR1S", cluster = bkh$UBound5K)

bkh$MonthInt <- bkh$HasCut * bkh$Month
bkh.ddtime <- plm(logBinCount ~ HasCut + MonthInt, data = bkh, index = c("UBound5K", "Month"),
                 model = "within", effect = "twoways")
coef_test(bkh.ddtime, vcov = "CR1S", cluster = bkh$UBound5K)