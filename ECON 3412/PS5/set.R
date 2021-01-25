library(haven)
library(sandwich)
library(ggplot2)
library(clubSandwich)
library(lmtest)
library(car)
library(plm)

robust_lm <- function(formula, data, conditions) {
  regression <- lm(formula, data = data)
  robust_coef <- coeftest(regression, vcov = vcovHC(regression, "HC1"))

  if (missing(conditions)) {
    return (list(regression, robust_coef))
  }

  f_test = linearHypothesis(regression, conditions, vcov = vcovHC(regression, "HC1"))

  return (list(regression, robust_coef, f_test))
}

demo <- read_dta("./income_democracy.dta")

is.pbalanced(demo)

demo$dem_ind[demo$country == "United States" & demo$year == 1965]
demo$dem_ind[demo$country == "Uruguay" & demo$year == 1965]
demo$dem_ind[demo$country == "Trinidad and Tobago" & demo$year == 1995]
demo$dem_ind[demo$country == "Venezuela, RB" & demo$year == 1995]

mean(demo$dem_ind, na.rm = TRUE)
sd(demo$dem_ind, na.rm = TRUE)
quantile(demo$dem_ind, c(0, 0.10, 0.25, 0.50, 0.75, 0.90, 1), na.rm = TRUE)

dem_gdp_nofe <- lm(dem_ind ~ log_gdppc, data = demo)
coef_test(dem_gdp_nofe, vcov = "CR1S", cluster = demo$code)

dem_gdp <- plm(dem_ind ~ log_gdppc, data = demo,
               index = c("country", "year"), model = "within")
coef_test(dem_gdp, vcov = "CR1S", cluster = demo$code)

demo$y60 <- as.integer(demo$year == 1960)
demo$y65 <- as.integer(demo$year == 1965)
demo$y70 <- as.integer(demo$year == 1970)
demo$y75 <- as.integer(demo$year == 1975)
demo$y80 <- as.integer(demo$year == 1980)
demo$y85 <- as.integer(demo$year == 1985)
demo$y90 <- as.integer(demo$year == 1990)
demo$y95 <- as.integer(demo$year == 1995)
dem_gdp_time <- plm(dem_ind ~ log_gdppc + y60 + y65 + y70 + y75 +
                 y80 + y85 + y90 + y95, data = demo,
               index = c("country", "year"), model = "within")
coef_test(dem_gdp, vcov = "CR1S", cluster = demo$code)

reg <- read_dta("./deregulate.dta")
reg$lnvc <- log(reg$vc)
reg$lnpl <- log(reg$pl)
reg$lnpf <- log(reg$pf)
reg$lnpm <- log(reg$pm)
reg$lnstage <- log(reg$stage)
ols_reg <- robust_lm(lnvc ~ reg + year + lnpl + lnpf + lnpm + lnstage, data = reg)
ols_reg[[2]]
fe_reg <- plm(lnvc ~ reg + year + lnpl + lnpf + lnpm + lnstage, data = reg,
              index = c("airline", "obs"), model = "within")
coeftest(fe_reg, vcov = vcovHC(fe_reg, type = "sss"))
summary(fe_reg)
## coeftest and coef_test are implemented in different packages,
## and as such have different names.
coef_test(fe_reg, vcov = "CR1S", cluster = reg$airline)

fe_reg <- plm(lnvc ~ reg + year + lnpl + lnpf + lnpm + lnstage +
                I(lnpl^2) + I(lnpf^2) + I(lnpm^2) + I(lnstage^2), data = reg,
              index = c("airline", "obs"), model = "within")
coef_test(fe_reg, vcov = "CR1S", cluster = reg$airline)
linearHypothesis(fe_reg, c("I(lnpl^2) = 0", "I(lnpf^2) = 0",
                           "I(lnpm^2) = 0", "I(lnstage^2) = 0"),
                 vcov = vcovCR(fe_reg, type = "CR1S", cluster = reg$airline),
                 test = "F")

q <- read_dta("./q.dta")
naive <- robust_lm(ikb ~ qb, q)
fe_reg_q <- plm(ikb ~ qb, data = q, index = c("cusip", "year"), model = "within")
coef_test(fe_reg_q, vcov = "CR1S", cluster = q$cusip)

q$qb_d <- 0
q$ikb_d <- 0
for (i in 1:length(q[[1]])) {
  q$ikb_d[i] = q$ikb[i] - mean(q$ikb[q$cusip == q$cusip[i]])
  q$qb_d[i] = q$qb[i] - mean(q$qb[q$cusip == q$cusip[i]])
}
ent_demean <- lm(ikb_d ~ qb_d, data = q)
coef_test(ent_demean, vcov = "CR1S", cluster = q$cusip)

coll_ikb <- c()
for (i in unique(q$year)) {
  coll_ikb <- append(coll_ikb, mean(q$ikb[q$year == i]))
}

q$y75 = as.integer(q$year == 75)
q$y76 = as.integer(q$year == 76)
q$y77 = as.integer(q$year == 77)
q$y78 = as.integer(q$year == 78)
q$y79 = as.integer(q$year == 79)
q$y80 = as.integer(q$year == 80)
q$y81 = as.integer(q$year == 81)
q$y82 = as.integer(q$year == 82)
q$y83 = as.integer(q$year == 83)
q$y84 = as.integer(q$year == 84)
q$y85 = as.integer(q$year == 85)
fe_reg_time_q <- plm(ikb ~ qb + y75 + y76 + y78 + y79 + y80 + y81 + y82 + y83 + y84 + y85, data = q, index = c("cusip", "year"), model = "within")
coef_test(fe_reg_time_q, vcov = "CR1S", cluster = q$cusip)

effects = c(0.01953, -0.03018, -0.01504,  0.01366,  0.03467,  0.00723,  0.00158, -0.04153, -0.03331, -0.00849, -0.01675)