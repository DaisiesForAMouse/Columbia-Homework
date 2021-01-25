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

apple <- read_dta("./APPLE.dta")
apple$ecobuy <- apple$ecolbs > 0
nrow(apple[apple$ecobuy == 1,])/nrow(apple)

apple.lpm <- lm(ecobuy ~ ecoprc + regprc + faminc + hhsize + educ + age, data = apple)
coeftest(apple.lpm, vcovCL)
linearHypothesis(apple.lpm, c("faminc = 0", "hhsize = 0", "educ = 0", "age = 0"), vcov = vcovCL)

apple$logfaminc <- log(apple$faminc)
apple.lpm <- lm(ecobuy ~ ecoprc + regprc + logfaminc + hhsize + educ + age, data = apple)
coeftest(apple.lpm, vcovCL)

nrow(apple[apple$ecobuy,])
nrow(apple[(predict(apple.lpm) >= 0.5) & apple$ecobuy,])
nrow(apple[!apple$ecobuy,])
nrow(apple[(predict(apple.lpm) <= 0.5) & !apple$ecobuy,])

htv <- read_dta("./HTV.dta")
htv.ols <- lm(lwage ~ educ, data = htv)
coeftest(htv.ols, vcovCL)

htv.olsc <- lm(lwage ~ educ + exper + I(exper^2) + ne + nc + west +
                 + ne18 + nc18 + west18 + urban + urban18,
               data = htv)
coeftest(htv.olsc, vcovCL)

htv.rfreg <- lm(educ ~ ctuit + exper + I(exper^2) + ne + nc + west +
                  + ne18 + nc18 + west18 + urban + urban18,
                data = htv)
coeftest(htv.rfreg, vcovCL)

htv.ivreg <- lm(lwage ~ predict(htv.rfreg) + exper + I(exper^2) + ne + nc + west +
                  + ne18 + nc18 + west18 + urban + urban18,
                data = htv)

htv.ivregreal <- ivreg(lwage ~ educ + exper + I(exper^2) + ne + nc + west +
                         + ne18 + nc18 + west18 + urban + urban18 | ctuit + exper +
                         I(exper^2) + ne + nc + west +
                         + ne18 + nc18 + west18 + urban + urban18,
                       data = htv)
coeftest(htv.ivregreal, vcovCL)

supas <- read_dta("./SUPAS.dta")
supas$young <- supas$YOB >= 68 & supas$YOB <= 72
supas$old <- supas$YOB >= 57 & supas$YOB <= 62
mean(supas[supas$young & supas$high,]$yeduc)
mean(supas[supas$young & !supas$high,]$yeduc)
mean(supas[supas$old & supas$high,]$yeduc)
mean(supas[supas$old & !supas$high,]$yeduc)
supas$treated <- supas$young * supas$high
filter <- supas[supas$young | supas$old,]
filter.did <- lm(yeduc ~ high + young + treated, data = filter)
coeftest(filter.did, vcovCL)

rob3516 <- filter$ROB == 3516
rob3219 <- filter$ROB == 3219
rob3578 <- filter$ROB == 3578
rob5309 <- filter$ROB == 5309
rob1671 <- filter$ROB == 1671
rob3518 <- filter$ROB == 3518
rob1275 <- filter$ROB == 1275
rob3172 <- filter$ROB == 3172
rob3175 <- filter$ROB == 3175
rob3174 <- filter$ROB == 3174
rob5304 <- filter$ROB == 5304
rob7319 <- filter$ROB == 7319
rob3173 <- filter$ROB == 3173
rob7317 <- filter$ROB == 7317
rob3273 <- filter$ROB == 3273
rob3211 <- filter$ROB == 3211
rob3573 <- filter$ROB == 3573
rob5308 <- filter$ROB == 5308
rob1375 <- filter$ROB == 1375
rob3310 <- filter$ROB == 3310
rob7308 <- filter$ROB == 7308
rob3171 <- filter$ROB == 3171
rob3402 <- filter$ROB == 3402
rob3206 <- filter$ROB == 3206
rob1171 <- filter$ROB == 1171
rob8103 <- filter$ROB == 8103
rob3302 <- filter$ROB == 3302
rob7103 <- filter$ROB == 7103
rob3213 <- filter$ROB == 3213
rob1303 <- filter$ROB == 1303
rob3203 <- filter$ROB == 3203
rob3520 <- filter$ROB == 3520
rob1272 <- filter$ROB == 1272
rob3312 <- filter$ROB == 3312
rob7305 <- filter$ROB == 7305
rob3311 <- filter$ROB == 3311
rob7371 <- filter$ROB == 7371
rob3208 <- filter$ROB == 3208
rob3503 <- filter$ROB == 3503
rob3404 <- filter$ROB == 3404
rob1607 <- filter$ROB == 1607
rob3313 <- filter$ROB == 3313
rob3322 <- filter$ROB == 3322
rob3374 <- filter$ROB == 3374
rob3471 <- filter$ROB == 3471
rob8171 <- filter$ROB == 8171
rob3572 <- filter$ROB == 3572
rob3519 <- filter$ROB == 3519
rob3371 <- filter$ROB == 3371
rob7318 <- filter$ROB == 7318
rob5106 <- filter$ROB == 5106
rob1305 <- filter$ROB == 1305
rob3209 <- filter$ROB == 3209
rob3515 <- filter$ROB == 3515
rob3505 <- filter$ROB == 3505
rob3577 <- filter$ROB == 3577
rob1302 <- filter$ROB == 1302
rob3372 <- filter$ROB == 3372
rob3308 <- filter$ROB == 3308
rob1308 <- filter$ROB == 1308
rob1604 <- filter$ROB == 1604
rob3571 <- filter$ROB == 3571
rob3212 <- filter$ROB == 3212
rob3403 <- filter$ROB == 3403
rob3514 <- filter$ROB == 3514
rob7104 <- filter$ROB == 7104
rob8102 <- filter$ROB == 8102
rob1102 <- filter$ROB == 1102
rob5310 <- filter$ROB == 5310
rob8101 <- filter$ROB == 8101
rob3309 <- filter$ROB == 3309
rob7372 <- filter$ROB == 7372
rob7312 <- filter$ROB == 7312
rob1307 <- filter$ROB == 1307
rob3306 <- filter$ROB == 3306
rob1211 <- filter$ROB == 1211
rob6471 <- filter$ROB == 6471
rob3576 <- filter$ROB == 3576
rob1701 <- filter$ROB == 1701
rob1571 <- filter$ROB == 1571
rob3373 <- filter$ROB == 3373
rob6472 <- filter$ROB == 6472
rob6309 <- filter$ROB == 6309
rob8104 <- filter$ROB == 8104
rob3506 <- filter$ROB == 3506
rob1371 <- filter$ROB == 1371
rob5105 <- filter$ROB == 5105
rob3504 <- filter$ROB == 3504
rob1372 <- filter$ROB == 1372
rob1703 <- filter$ROB == 1703
rob7306 <- filter$ROB == 7306
rob1373 <- filter$ROB == 1373
rob1301 <- filter$ROB == 1301
rob7302 <- filter$ROB == 7302
rob3376 <- filter$ROB == 3376
rob1306 <- filter$ROB == 1306
rob1376 <- filter$ROB == 1376
rob7307 <- filter$ROB == 7307
rob1271 <- filter$ROB == 1271
rob3324 <- filter$ROB == 3324
rob1771 <- filter$ROB == 1771
rob6301 <- filter$ROB == 6301
rob3274 <- filter$ROB == 3274
rob7102 <- filter$ROB == 7102
rob7315 <- filter$ROB == 7315
rob1304 <- filter$ROB == 1304
rob3517 <- filter$ROB == 3517
rob3328 <- filter$ROB == 3328
rob7172 <- filter$ROB == 7172
rob7101 <- filter$ROB == 7101
rob7310 <- filter$ROB == 7310
rob1204 <- filter$ROB == 1204
rob3575 <- filter$ROB == 3575
rob3510 <- filter$ROB == 3510
rob3210 <- filter$ROB == 3210
rob1471 <- filter$ROB == 1471
rob7313 <- filter$ROB == 7313
rob7404 <- filter$ROB == 7404
rob5302 <- filter$ROB == 5302
rob5312 <- filter$ROB == 5312
rob7304 <- filter$ROB == 7304
rob3521 <- filter$ROB == 3521
rob5307 <- filter$ROB == 5307
rob7311 <- filter$ROB == 7311
rob3275 <- filter$ROB == 3275
rob1608 <- filter$ROB == 1608
rob5303 <- filter$ROB == 5303
rob7171 <- filter$ROB == 7171
rob7301 <- filter$ROB == 7301
rob3401 <- filter$ROB == 3401
rob5305 <- filter$ROB == 5305
rob7314 <- filter$ROB == 7314
rob1106 <- filter$ROB == 1106
rob6271 <- filter$ROB == 6271
rob7173 <- filter$ROB == 7173
rob1672 <- filter$ROB == 1672
rob1374 <- filter$ROB == 1374
rob3501 <- filter$ROB == 3501
rob5306 <- filter$ROB == 5306
rob6202 <- filter$ROB == 6202
rob7320 <- filter$ROB == 7320
rob5301 <- filter$ROB == 5301
rob1104 <- filter$ROB == 1104
rob7321 <- filter$ROB == 7321
rob7316 <- filter$ROB == 7316
rob6307 <- filter$ROB == 6307
rob7303 <- filter$ROB == 7303
rob6205 <- filter$ROB == 6205
rob1172 <- filter$ROB == 1172
rob5311 <- filter$ROB == 5311
rob5201 <- filter$ROB == 5201
rob3529 <- filter$ROB == 3529
rob3201 <- filter$ROB == 3201
rob3528 <- filter$ROB == 3528
rob3216 <- filter$ROB == 3216
rob1210 <- filter$ROB == 1210
rob3204 <- filter$ROB == 3204
rob6101 <- filter$ROB == 6101
rob3304 <- filter$ROB == 3304
rob3205 <- filter$ROB == 3205
rob1871 <- filter$ROB == 1871
rob3508 <- filter$ROB == 3508
rob3513 <- filter$ROB == 3513
rob1405 <- filter$ROB == 1405
rob3523 <- filter$ROB == 3523
rob3215 <- filter$ROB == 3215
rob1274 <- filter$ROB == 1274
rob5104 <- filter$ROB == 5104
rob3316 <- filter$ROB == 3316
rob3574 <- filter$ROB == 3574
rob6104 <- filter$ROB == 6104
rob5202 <- filter$ROB == 5202
rob6105 <- filter$ROB == 6105
rob3218 <- filter$ROB == 3218
rob3321 <- filter$ROB == 3321
rob1603 <- filter$ROB == 1603
rob1101 <- filter$ROB == 1101
rob5107 <- filter$ROB == 5107
rob3220 <- filter$ROB == 3220
rob1202 <- filter$ROB == 1202
rob6403 <- filter$ROB == 6403
rob7202 <- filter$ROB == 7202
rob1103 <- filter$ROB == 1103
rob6106 <- filter$ROB == 6106
rob5101 <- filter$ROB == 5101
rob5205 <- filter$ROB == 5205
rob1501 <- filter$ROB == 1501
rob1108 <- filter$ROB == 1108
rob5206 <- filter$ROB == 5206
rob3526 <- filter$ROB == 3526
rob3323 <- filter$ROB == 3323
rob6302 <- filter$ROB == 6302
rob6171 <- filter$ROB == 6171
rob1503 <- filter$ROB == 1503
rob6308 <- filter$ROB == 6308
rob6404 <- filter$ROB == 6404
rob1602 <- filter$ROB == 1602
rob3217 <- filter$ROB == 3217
rob3527 <- filter$ROB == 3527
rob3507 <- filter$ROB == 3507
rob1505 <- filter$ROB == 1505
rob3317 <- filter$ROB == 3317
rob3327 <- filter$ROB == 3327
rob1201 <- filter$ROB == 1201
rob1802 <- filter$ROB == 1802
rob6204 <- filter$ROB == 6204
rob3320 <- filter$ROB == 3320
rob1504 <- filter$ROB == 1504
rob5271 <- filter$ROB == 5271
rob1402 <- filter$ROB == 1402
rob3509 <- filter$ROB == 3509
rob6103 <- filter$ROB == 6103
rob1605 <- filter$ROB == 1605
rob3207 <- filter$ROB == 3207
rob1401 <- filter$ROB == 1401
rob6305 <- filter$ROB == 6305
rob3318 <- filter$ROB == 3318
rob3272 <- filter$ROB == 3272
rob5108 <- filter$ROB == 5108
rob3319 <- filter$ROB == 3319
rob6102 <- filter$ROB == 6102
rob3214 <- filter$ROB == 3214
rob6201 <- filter$ROB == 6201
rob7204 <- filter$ROB == 7204
rob3305 <- filter$ROB == 3305
rob1208 <- filter$ROB == 1208
rob3315 <- filter$ROB == 3315
rob5102 <- filter$ROB == 5102
rob6306 <- filter$ROB == 6306
rob6203 <- filter$ROB == 6203
rob7309 <- filter$ROB == 7309
rob5203 <- filter$ROB == 5203
rob6402 <- filter$ROB == 6402
rob1502 <- filter$ROB == 1502
rob3307 <- filter$ROB == 3307
rob1205 <- filter$ROB == 1205
rob1206 <- filter$ROB == 1206
rob3522 <- filter$ROB == 3522
rob1207 <- filter$ROB == 1207
rob1273 <- filter$ROB == 1273
rob1404 <- filter$ROB == 1404
rob3271 <- filter$ROB == 3271
rob1107 <- filter$ROB == 1107
rob7201 <- filter$ROB == 7201
rob3524 <- filter$ROB == 3524
rob3301 <- filter$ROB == 3301
rob3314 <- filter$ROB == 3314
rob3326 <- filter$ROB == 3326
rob1203 <- filter$ROB == 1203
rob3202 <- filter$ROB == 3202
rob1702 <- filter$ROB == 1702
rob3303 <- filter$ROB == 3303
rob1105 <- filter$ROB == 1105
rob1403 <- filter$ROB == 1403
rob3329 <- filter$ROB == 3329
rob6304 <- filter$ROB == 6304
rob5103 <- filter$ROB == 5103
rob3502 <- filter$ROB == 3502
rob1601 <- filter$ROB == 1601
rob3525 <- filter$ROB == 3525
rob7401 <- filter$ROB == 7401
rob3325 <- filter$ROB == 3325
rob7402 <- filter$ROB == 7402
rob3375 <- filter$ROB == 3375
rob1276 <- filter$ROB == 1276
rob3512 <- filter$ROB == 3512
rob1209 <- filter$ROB == 1209
rob3511 <- filter$ROB == 3511
rob6303 <- filter$ROB == 6303
rob1803 <- filter$ROB == 1803
rob6371 <- filter$ROB == 6371
rob1801 <- filter$ROB == 1801
rob7203 <- filter$ROB == 7203
rob1606 <- filter$ROB == 1606
rob5171 <- filter$ROB == 5171
rob5204 <- filter$ROB == 5204
rob7403 <- filter$ROB == 7403
rob1472 <- filter$ROB == 1472
rob6401 <- filter$ROB == 6401
rob1804 <- filter$ROB == 1804

yob68 <- filter$YOB == 68
yob61 <- filter$YOB == 61
yob60 <- filter$YOB == 60
yob57 <- filter$YOB == 57
yob72 <- filter$YOB == 72
yob71 <- filter$YOB == 71
yob62 <- filter$YOB == 62
yob70 <- filter$YOB == 70
yob58 <- filter$YOB == 58
yob69 <- filter$YOB == 69
yob59 <- filter$YOB == 59

filter.fe <- lm(filter$yeduc ~ filter$treated + rob3516 + rob3219 + rob3578 + rob5309 + rob1671 + rob3518 + rob1275 + rob3172 + rob3175 + rob3174 + rob5304 + rob7319 + rob3173 + rob7317 + rob3273 + rob3211 + rob3573 + rob5308 + rob1375 + rob3310 + rob7308 + rob3171 + rob3402 + rob3206 + rob1171 + rob8103 + rob3302 + rob7103 + rob3213 + rob1303 + rob3203 + rob3520 + rob1272 + rob3312 + rob7305 + rob3311 + rob7371 + rob3208 + rob3503 + rob3404 + rob1607 + rob3313 + rob3322 + rob3374 + rob3471 + rob8171 + rob3572 + rob3519 + rob3371 + rob7318 + rob5106 + rob1305 + rob3209 + rob3515 + rob3505 + rob3577 + rob1302 + rob3372 + rob3308 + rob1308 + rob1604 + rob3571 + rob3212 + rob3403 + rob3514 + rob7104 + rob8102 + rob1102 + rob5310 + rob8101 + rob3309 + rob7372 + rob7312 + rob1307 + rob3306 + rob1211 + rob6471 + rob3576 + rob1701 + rob1571 + rob3373 + rob6472 + rob6309 + rob8104 + rob3506 + rob1371 + rob5105 + rob3504 + rob1372 + rob1703 + rob7306 + rob1373 + rob1301 + rob7302 + rob3376 + rob1306 + rob1376 + rob7307 + rob1271 + rob3324 + rob1771 + rob6301 + rob3274 + rob7102 + rob7315 + rob1304 + rob3517 + rob3328 + rob7172 + rob7101 + rob7310 + rob1204 + rob3575 + rob3510 + rob3210 + rob1471 + rob7313 + rob7404 + rob5302 + rob5312 + rob7304 + rob3521 + rob5307 + rob7311 + rob3275 + rob1608 + rob5303 + rob7171 + rob7301 + rob3401 + rob5305 + rob7314 + rob1106 + rob6271 + rob7173 + rob1672 + rob1374 + rob3501 + rob5306 + rob6202 + rob7320 + rob5301 + rob1104 + rob7321 + rob7316 + rob6307 + rob7303 + rob6205 + rob1172 + rob5311 + rob5201 + rob3529 + rob3201 + rob3528 + rob3216 + rob1210 + rob3204 + rob6101 + rob3304 + rob3205 + rob1871 + rob3508 + rob3513 + rob1405 + rob3523 + rob3215 + rob1274 + rob5104 + rob3316 + rob3574 + rob6104 + rob5202 + rob6105 + rob3218 + rob3321 + rob1603 + rob1101 + rob5107 + rob3220 + rob1202 + rob6403 + rob7202 + rob1103 + rob6106 + rob5101 + rob5205 + rob1501 + rob1108 + rob5206 + rob3526 + rob3323 + rob6302 + rob6171 + rob1503 + rob6308 + rob6404 + rob1602 + rob3217 + rob3527 + rob3507 + rob1505 + rob3317 + rob3327 + rob1201 + rob1802 + rob6204 + rob3320 + rob1504 + rob5271 + rob1402 + rob3509 + rob6103 + rob1605 + rob3207 + rob1401 + rob6305 + rob3318 + rob3272 + rob5108 + rob3319 + rob6102 + rob3214 + rob6201 + rob7204 + rob3305 + rob1208 + rob3315 + rob5102 + rob6306 + rob6203 + rob7309 + rob5203 + rob6402 + rob1502 + rob3307 + rob1205 + rob1206 + rob3522 + rob1207 + rob1273 + rob1404 + rob3271 + rob1107 + rob7201 + rob3524 + rob3301 + rob3314 + rob3326 + rob1203 + rob3202 + rob1702 + rob3303 + rob1105 + rob1403 + rob3329 + rob6304 + rob5103 + rob3502 + rob1601 + rob3525 + rob7401 + rob3325 + rob7402 + rob3375 + rob1276 + rob3512 + rob1209 + rob3511 + rob6303 + rob1803 + rob6371 + rob1801 + rob7203 + rob1606 + rob5171 + rob5204 + rob7403 + rob1472 + rob6401 + rob1804 + yob68 + yob61 + yob60 + yob57 + yob72 + yob71 + yob62 + yob70 + yob58 + yob69 + yob59)

growth <- read_dta("./gr_ur.dta")
growth <- gen_lags(growth, growth, 2)
growth <- gen_lags(growth, unrate, 2)
growth.ar2 <- lm(growth ~ growthlag1 + growthlag2, data = growth)
coeftest(growth.ar2, vcovCL)
growth.adl11 <- lm(growth ~ growthlag1 + unratelag1, data = growth)
coeftest(growth.adl11, vcovCL)

oo <- read_dta("Olive_Oil.dta")
oo <- gen_lags(oo, gdd, 4)
oo.dyn <- lm(pc_price ~ gdd + gddlag1 + gddlag2 + gddlag3 + gddlag4, data = oo)
coeftest(oo.dyn, vcov = NeweyWest(oo.dyn, lag = 4))

oo$delta_0 <- oo$gdd - oo$gddlag1
oo$delta_1 <- oo$gddlag1 - oo$gddlag2
oo$delta_2 <- oo$gddlag2 - oo$gddlag3
oo$delta_3 <- oo$gddlag3 - oo$gddlag4

oo.cumdyn <- lm(pc_price ~ delta_0 + delta_1 + delta_2 + delta_3 + gddlag4, data = oo)
coeftest(oo.cumdyn, vcov = NeweyWest(oo.cumdyn, lag = 4))

## growth <- read_dta("growth.dta")
## growth <- gen_lags(growth, growth, 4)
## growth <- gen_lags(growth, per_ip, 4)

## growth.ar1 <- lm(growth ~ growthlag1, data = growth)
## coeftest(growth.ar1, vcovCL)

## growth.ar4 <- lm(growth ~ growthlag1 + growthlag2 +
##                    growthlag3 + growthlag4,
##                  data = growth)
## coeftest(growth.ar4, vcovCL)

## growth.adl11 <- lm(growth ~ growthlag1 + per_iplag1,
##                    data = growth)
## coeftest(growth.adl11, vcovCL)

## growth.adl44 <- lm(growth ~ growthlag1 + growthlag2 + growthlag3 + growthlag4 +
##                      per_iplag1 + per_iplag2 + per_iplag3 + per_iplag4,
##                    data = growth)
## coeftest(growth.adl44, vcovCL)

## linearHypothesis(growth.adl44, c("per_iplag1 = 0", "per_iplag2 = 0",
##                                  "per_iplag3 = 0", "per_iplag4 = 0"),
##                  vcov = vcovHC(growth.adl44, "HC1"))

## wage <- read_dta("WAGEPRC.dta")

## wage.ols <- lm(gprice ~ gwage, data = wage)
## coeftest(wage.ols, vcovCL)

## wage.tsols <- lm(gprice ~ gwage + gwage_1 + gwage_2 + gwage_3 + gwage_4 +
##                    gwage_5 + gwage_6 + gwage_7 + gwage_8 + gwage_9 +
##                    gwage_10 + gwage_11 + gwage_12,
##                  data = wage)
## coeftest(wage.tsols, vcov = NeweyWest(wage.tsols))

## coefs <- coeftest(wage.tsols, vcov = NeweyWest(wage.tsols))
## coefs_upper <- c()
## coefs_lower <- c()
## coefs_est <- c()

## for (i in 1:14) {
##   coefs_upper <- append(coefs_upper, coefs[i, 1] + 1.96 * coefs[i, 2])
##   coefs_lower <- append(coefs_lower, coefs[i, 1] - 1.96 * coefs[i, 2])
##   coefs_est <- append(coefs_est, coefs[i, 1])
## }

## ggplot() +
##     geom_point(aes(x = 1:14, y = coefs_est)) +
##     geom_line() +
##   geom_errorbar(aes(x = 1:14, y = coefs_est, ymin = coefs_lower, ymax = coefs_upper)) +
##   scale_x_discrete(name = "Parameter", limits = row.names(coefs))

## wage$delta_0 <- wage$gwage - wage$gwage_1
## wage$delta_1 <- wage$gwage_1 - wage$gwage_2
## wage$delta_2 <- wage$gwage_2 - wage$gwage_3
## wage$delta_3 <- wage$gwage_3 - wage$gwage_4
## wage$delta_4 <- wage$gwage_4 - wage$gwage_5
## wage$delta_5 <- wage$gwage_5 - wage$gwage_6
## wage$delta_6 <- wage$gwage_6 - wage$gwage_7
## wage$delta_7 <- wage$gwage_7 - wage$gwage_8
## wage$delta_8 <- wage$gwage_8 - wage$gwage_9
## wage$delta_9 <- wage$gwage_9 - wage$gwage_10
## wage$delta_10 <- wage$gwage_10 - wage$gwage_11
## wage$delta_11 <- wage$gwage_11 - wage$gwage_12

## wage.tscumols <- lm(gprice ~ delta_0 + delta_1 + delta_2 + delta_3 + delta_4 +
##                       delta_5 + delta_6 + delta_7 + delta_8 + delta_9 +
##                       delta_10 + delta_11 + gwage_12,
##                     data = wage)
## coeftest(wage.tscumols, vcov = NeweyWest(wage.tscumols))