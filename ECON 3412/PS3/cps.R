library(haven)
library(sandwich)
library(lmtest)

cps_data <- read_dta("CPS2015.dta")
ahe <- cps_data$ahe
female <- cps_data$female
nonrobust_ols <- lm(ahe ~ female)
robust_ols <- coeftest(nonrobust_ols, vcov = vcovHC(nonrobust_ols, "HC1"))

message("Nonrobust:")
print(summary(nonrobust_ols))
message("Robust:")
print(robust_ols)

age <- cps_data$age
nonrobust_ols_age <- lm(ahe ~ female + age)
robust_ols_age <- coeftest(nonrobust_ols_age, vcov = vcovHC(nonrobust_ols_age, "HC1"))

message("Nonrobust with age:")
print(summary(nonrobust_ols_age))
message("Robust with age:")
print(robust_ols_age)


bachelor <- cps_data$bachelor
nonrobust_ols_bach <- lm(ahe ~ female + bachelor)
robust_ols_bach <- coeftest(nonrobust_ols_bach, vcov = vcovHC(nonrobust_ols_bach, "HC1"))

message("Nonrobust with bachelors:")
print(summary(nonrobust_ols_bach))
message("Robust with bachelors:")
print(robust_ols_bach)