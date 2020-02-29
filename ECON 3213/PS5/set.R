library(xtable)

n = 0.01
g = 0.02
delta = 0.0698
sigma = 0.25
alpha = 0.5

solow <- function(gdp_cap, e, years) {
  gdp_caps <- double(years)
  es <- double(years)
  capital_caps <- double(years)
  growths <- double(years)

  gdp_caps[1] <- gdp_cap
  es[1] <- e
  capital_caps[1] <- ((gdp_cap) / (e ^ (1 - alpha))) ^ (1 / alpha)

  for (year in 2:years) {
    es[year] <- es[year - 1] * (1 + g)
    capital_caps[year] <- (capital_caps[year - 1] * (1 - delta) +
                           gdp_caps[year - 1] * sigma) / (1 + n)
    gdp_caps[year] <- (capital_caps[year] ^ alpha) * (es[year] ^ (1 - alpha))
    growths[year] <- (gdp_caps[year] / gdp_cap) ^ (1/(year - 1))
  }

  tech <- (log(es) - log(e)) / (log(gdp_caps) - log(gdp_cap))

  return (list(gdp=gdp_caps, tech=tech, growth=growths))
}

A <- solow(2.5, 1, 26)
B <- solow(0.25, 1, 26)

part_three <- data.frame(A$gdp, A$growth, A$tech, B$gdp, B$growth, B$tech)

colnames(part_three) <- c("A GDP/Capita", "A % growth", "A % from tech",
                          "B GDP/Capita", "B % growth", "B % from tech")

xtable(part_three, type="latex")
