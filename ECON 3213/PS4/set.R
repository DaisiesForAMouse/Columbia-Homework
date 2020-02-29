library(xtable)

A_computed <- 1403.5
us_k <- 116015.40
ch_k <- 29.22

part_three_data <- function(k, alpha, delta, sigma, A, years) {
  ## Preallocate size because R copies the entire vector...
  k_t <- double(years)
  y_t <- double(years)
  i_t <- double(years)

  k_t[1] <- k
  y_t[1] <- A * (k ^ alpha)
  i_t[1] <- sigma * y_t[1]

  for (year in 2:years) {
    k_t[year] <- k_t[year - 1] * (1 - delta) + i_t[year - 1]
    y_t[year] <- A * (k_t[year] ^ alpha)
    i_t[year] <- sigma * y_t[year]
  }

  return (list(capital=k_t, invest_per_capital=(i_t / k_t), output=y_t))
}

us_data <- part_three_data(us_k, 0.3, 0.1, 0.25, A_computed, 40)
ch_data <- part_three_data(ch_k, 0.3, 0.1, 0.25, A_computed, 40)

part_three <- data.frame(us_data$capital, ch_data$capital,
                         us_data$invest_per_capital, ch_data$invest_per_capital,
                         us_data$output, ch_data$output, us_data$output / ch_data$output)

colnames(part_three) <- c("US k_i", "CH k_t",
                          "US i_t/k_t", "CH i_t/k_t",
                          "US y_t", "CH y_t", "y^{us}_t / y^{ch}_t")

part_four <- data.frame(us_data$output / ch_data$output)

colnames(part_four) <- c("y^{us}_t / y^{ch}_t")

xtable(part_three, type="latex")
