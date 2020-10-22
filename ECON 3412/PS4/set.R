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

## plot_two_way <- function
terror_dta <- read_dta("./terrorism.dta")
terror_dta <- terror_dta[terror_dta$ftmpop != 0,]
terror_dta$lnftmpop <- log(terror_dta$ftmpop)
terror_dta$lngdppc <- log(terror_dta$gdppc)

t2_reg = list()
t2_reg[[1]] <- robust_lm(lnftmpop ~ lngdppc, terror_dta)
t2_reg[[2]] <- robust_lm(lnftmpop ~ lngdppc + lackpf, terror_dta)
t2_reg[[3]] <- robust_lm(lnftmpop ~ lngdppc + I(lngdppc ^ 2) +
                           lackpf + I(lackpf ^ 2), terror_dta)
t2_reg[[4]] <- robust_lm(lnftmpop ~ lngdppc + lackpf +
                           I(lackpf ^ 2) + ethnic + religion, terror_dta)
t2_reg[[5]] <- robust_lm(lnftmpop ~ lngdppc + lackpf +
                           I(lackpf ^ 2) + ethnic + religion +
                           mideast + latinam + easteurope + africa + eastasia,
                         terror_dta)

linearHypothesis(t2_reg[[3]][[1]],
                 c("lngdppc = 0", "I(lngdppc^2) = 0"),
                 vcov = vcovHC(t2_reg[[3]][[1]], "HC1"))
linearHypothesis(t2_reg[[3]][[1]],
                 c("lackpf = 0", "I(lackpf^2) = 0"),
                 vcov = vcovHC(t2_reg[[3]][[1]], "HC1"))
linearHypothesis(t2_reg[[4]][[1]],
                 c("lackpf = 0", "I(lackpf^2) = 0"),
                 vcov = vcovHC(t2_reg[[4]][[1]], "HC1"))
linearHypothesis(t2_reg[[4]][[1]],
                 c("ethnic = 0", "religion = 0"),
                 vcov = vcovHC(t2_reg[[4]][[1]], "HC1"))
linearHypothesis(t2_reg[[5]][[1]],
                 c("lackpf = 0", "I(lackpf^2) = 0"),
                 vcov = vcovHC(t2_reg[[5]][[1]], "HC1"))
linearHypothesis(t2_reg[[5]][[1]],
                 c("ethnic = 0", "religion = 0"),
                 vcov = vcovHC(t2_reg[[5]][[1]], "HC1"))
linearHypothesis(t2_reg[[5]][[1]],
                 c("latinam = 0", "easteurope = 0", "africa = 0", "eastasia = 0"),
                 vcov = vcovHC(t2_reg[[5]][[1]], "HC1"))

terror_dta$higdppc <- as.integer(terror_dta$gdppc > median(terror_dta$gdppc, na.rm=TRUE))

t3_reg = list()
t3_reg[[1]] <- robust_lm(lnftmpop ~ higdppc + lackpf +
                           i(higdppc * lackpf), terror_dta)
t3_reg[[2]] <- robust_lm(lnftmpop ~ higdppc + lackpf + i(lackpf ^ 2) +
                           i(higdppc * lackpf) + i(higdppc * lackpf ^ 2), terror_dta)
t3_reg[[3]] <- robust_lm(lnftmpop ~ higdppc + lackpf + i(lackpf ^ 2) +
                           ethnic + religion + i(higdppc * ethnic) + i(higdppc * religion), terror_dta)
t3_reg[[4]] <- robust_lm(lnftmpop ~ higdppc + lackpf + i(lackpf ^ 2) +
                           ethnic + religion + i(higdppc * ethnic) + i(higdppc * religion) +
                           mideast + latinam + easteurope + africa + eastasia, terror_dta)

linearhypothesis(t3_reg[[2]][[1]],
                 c("i(higdppc * lackpf) = 0", "i(higdppc * lackpf^2) = 0"),
                 vcov = vcovhc(t3_reg[[2]][[1]]))
linearhypothesis(t3_reg[[2]][[1]],
                 c("lackpf = 0", "i(higdppc * lackpf^2) = 0"),
                 vcov = vcovhc(t3_reg[[2]][[1]]))
linearhypothesis(t3_reg[[2]][[1]],
                 c("lackpf = 0", "i(lackpf^2) = 0"),
                 vcov = vcovhc(t3_reg[[2]][[1]]))
linearhypothesis(t3_reg[[3]][[1]],
                 c("lackpf = 0", "i(lackpf^2) = 0"),
                 vcov = vcovhc(t3_reg[[3]][[1]]))
linearhypothesis(t3_reg[[3]][[1]],
                 c("i(higdppc * ethnic) = 0", "i(higdppc * religion) = 0"),
                 vcov = vcovhc(t3_reg[[3]][[1]]))
linearhypothesis(t3_reg[[3]][[1]],
                 c("i(higdppc * ethnic) = 0", "i(higdppc * religion) = 0",
                   "ethnic = 0", "religion = 0"),
                 vcov = vcovhc(t3_reg[[3]][[1]]))
linearhypothesis(t3_reg[[4]][[1]],
                 c("lackpf = 0", "i(lackpf^2) = 0"),
                 vcov = vcovhc(t3_reg[[4]][[1]]))
linearhypothesis(t3_reg[[4]][[1]],
                 c("i(higdppc * ethnic) = 0", "i(higdppc * religion) = 0"),
                 vcov = vcovhc(t3_reg[[4]][[1]]))
linearhypothesis(t3_reg[[4]][[1]],
                 c("i(higdppc * ethnic) = 0", "i(higdppc * religion) = 0",
                   "ethnic = 0", "religion = 0"),
                 vcov = vcovhc(t3_reg[[4]][[1]]))
linearhypothesis(t3_reg[[4]][[1]],
                 c("latinam = 0", "easteurope = 0",
                   "eastasia = 0", "africa = 0"),
                 vcov = vcovhc(t3_reg[[4]][[1]]))