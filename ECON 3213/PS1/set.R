growth <- function(start, end, first, last) {
  return ((end / start) ^ (1 / (last - first)))
}

present_value <- function(start, end, first, last, years_later, discount) {
  g <- growth(start, end, first, last)
  years <- last - first
  value <- 0
  for (i in 0:years) {
    value <- (value + (start * (g ^ i) / ((1 + discount) ^ (i + years_later))))
  }
  return (value)
}

project <- function(start, years, growth) {
  return ((start) * (growth ^ years))
}

"Problem 2"
cat("Irish growth is ", (growth(1775, 2736, 1870, 1913) - 1) * 100, "%.\n")
cat("American growth is ", (growth(2445, 5301, 1870, 1913) - 1) * 100, "%.\n")
cat("Argentenian growth is ", (growth(1311, 3797, 1870, 1913) - 1) * 100, "%.\n")

"Problem 3"
cat("Irish present value is ", present_value(1775, 2736, 1870, 1913, 0, 0.03), ".\n")
cat("American present value is ", present_value(2445, 5301, 1870, 1913, 0, 0.03) , ".\n")
cat("Argentenian present value is ", present_value(1311, 3797, 1870, 1913, 0, 0.03), ".\n")

"Problem 4"
ire_proj <- project(2736, 1929 - 1913, growth(1775, 2736, 1870, 1913))
ame_proj <- project(5301, 1929 - 1913, growth(2445, 5301, 1870, 1913))
arg_proj <- project(3797, 1929 - 1913, growth(1311, 3797, 1870, 1913))
cat("Projected rGDP for Ireland is ", ire_proj, ".\n")
cat("Projected rGDP for America is ", ame_proj, ".\n")
cat("Projected rGDP for Argentina is ", arg_proj, ".\n")

"Problem 5"
cat("Irish present value is ", present_value(2736, ire_proj, 1913, 1929, 0, 0.03), ".\n")
cat("American present value is ", present_value(5301, ame_proj, 1913, 1929, 0, 0.03) , ".\n")
cat("Argentenian present value is ", present_value(3797, arg_proj, 1913, 1929, 0, 0.03), ".\n")

"Problem 6"
cat("Irish present value is ", present_value(1775, 2736, 1870, 1913, 0, 0.03) +
                               present_value(2736, 2824, 1913, 1929, 1913 - 1870, 0.03) -
                               (2736 / ((1 + 0.03) ^ (1913 - 1870))), ".\n")
cat("American present value is ", present_value(2445, 5301, 1870, 1913, 0, 0.03) +
                                  present_value(5301, 6899, 1913, 1929, 1913 - 1870, 0.03) -
                                  (5301 / ((1 + 0.03) ^ (1913 - 1870))), ".\n")
cat("Argentenian present value is ", present_value(1311, 3797, 1870, 1913, 0, 0.03) +
                                     present_value(3797, 4367, 1913, 1929, 1913 - 1870, 0.03) -
                                     (3797 / ((1 + 0.03) ^ (1913 - 1870))), ".\n")

