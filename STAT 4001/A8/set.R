del <- function(p) {
  S <- c(0,1,7,8,9,10,11,12,13,14,15,16,17,18,19,20)
  ret <- 0
  for (s in S) {
    ret = ret + choose(20, s) * p^(s) * (1-p)^(20-s)
  }

  return (ret)
}
