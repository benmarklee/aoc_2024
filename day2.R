
# AoC 2024 - Day 2

x <- strsplit(readLines("input.txt"), " ")

# Part 1
f <- \(x) {
  y <- diff(as.numeric(x))
  z <- sign(y)
  length(z) == sum(z == z[1]) & max(abs(y)) < 4 & min(abs(y)) > 0
}
sum(sapply(x, f))

# Part 2
sum(sapply(x, \(x) {
  f(x) | {
    for (i in 1:length(x))
      if (f(x[-i])) return(T)
    F
  }
}))

