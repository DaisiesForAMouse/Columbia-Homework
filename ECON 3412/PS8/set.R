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

## growth <- ts(read_dta("growth.dta"), frequency = 4, start = c(1948,1))
## lag(ts(growth, frequency = 4, start = c(1948,1)), k = 1)

gen_lags <- function(df, var, lags) {
  data <- eval(substitute(var), df)
  for (i in 1:lags) {
    data <- append(data, NA, after = 0)[1:length(data)]
    names <- append(colnames(df), paste(deparse(substitute(var)), "lag", i, sep = ""))
    df <- cbind(df, data)
    colnames(df) <- names
  }
  return (df)
}

growth <- read_dta("growth.dta")
growth <- gen_lags(growth, growth, 4)
growth <- gen_lags(growth, per_ip, 4)

growth.ar1 <- lm(growth ~ growthlag1, data = growth)
coeftest(growth.ar1, vcovCL)

growth.ar4 <- lm(growth ~ growthlag1 + growthlag2 +
                   growthlag3 + growthlag4,
                 data = growth)
coeftest(growth.ar4, vcovCL)

growth.adl11 <- lm(growth ~ growthlag1 + per_iplag1,
                   data = growth)
coeftest(growth.adl11, vcovCL)

growth.adl44 <- lm(growth ~ growthlag1 + growthlag2 + growthlag3 + growthlag4 +
                     per_iplag1 + per_iplag2 + per_iplag3 + per_iplag4,
                   data = growth)
coeftest(growth.adl44, vcovCL)

linearHypothesis(growth.adl44, c("per_iplag1 = 0", "per_iplag2 = 0",
                                 "per_iplag3 = 0", "per_iplag4 = 0"),
                 vcov = vcovHC(growth.adl44, "HC1"))

wage <- read_dta("WAGEPRC.dta")

wage.ols <- lm(gprice ~ gwage, data = wage)
coeftest(wage.ols, vcovCL)

wage.tsols <- lm(gprice ~ gwage + gwage_1 + gwage_2 + gwage_3 + gwage_4 +
                   gwage_5 + gwage_6 + gwage_7 + gwage_8 + gwage_9 +
                   gwage_10 + gwage_11 + gwage_12,
                 data = wage)
coeftest(wage.tsols, vcov = NeweyWest(wage.tsols))

coefs <- coeftest(wage.tsols, vcov = NeweyWest(wage.tsols))
coefs_upper <- c()
coefs_lower <- c()
coefs_est <- c()

for (i in 1:14) {
  coefs_upper <- append(coefs_upper, coefs[i, 1] + 1.96 * coefs[i, 2])
  coefs_lower <- append(coefs_lower, coefs[i, 1] - 1.96 * coefs[i, 2])
  coefs_est <- append(coefs_est, coefs[i, 1])
}

ggplot() +
    geom_point(aes(x = 1:14, y = coefs_est)) +
    geom_line() +
  geom_errorbar(aes(x = 1:14, y = coefs_est, ymin = coefs_lower, ymax = coefs_upper)) +
  scale_x_discrete(name = "Parameter", limits = row.names(coefs))

wage$delta_0 <- wage$gwage - wage$gwage_1
wage$delta_1 <- wage$gwage_1 - wage$gwage_2
wage$delta_2 <- wage$gwage_2 - wage$gwage_3
wage$delta_3 <- wage$gwage_3 - wage$gwage_4
wage$delta_4 <- wage$gwage_4 - wage$gwage_5
wage$delta_5 <- wage$gwage_5 - wage$gwage_6
wage$delta_6 <- wage$gwage_6 - wage$gwage_7
wage$delta_7 <- wage$gwage_7 - wage$gwage_8
wage$delta_8 <- wage$gwage_8 - wage$gwage_9
wage$delta_9 <- wage$gwage_9 - wage$gwage_10
wage$delta_10 <- wage$gwage_10 - wage$gwage_11
wage$delta_11 <- wage$gwage_11 - wage$gwage_12

wage.tscumols <- lm(gprice ~ delta_0 + delta_1 + delta_2 + delta_3 + delta_4 +
                      delta_5 + delta_6 + delta_7 + delta_8 + delta_9 +
                      delta_10 + delta_11 + gwage_12,
                    data = wage)
coeftest(wage.tscumols, vcov = NeweyWest(wage.tscumols))