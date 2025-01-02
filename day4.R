
# AoC 2024 - Day 4

# Part 1
d <- matrix(unlist(sapply(readLines("input.txt"), strsplit, "")), nc = 140, by = T)
n <- 0
rm <- nrow(d)
cm <- ncol(d)
f <- \(s) paste0(s, collapse = "") %in% c("XMAS", "SAMX")

for (ri in 1:rm) {
  for (ci in 1:cm) {
    n <-
    n +
      sum(c(
        if(ci <= cm - 3) f(d[ri, ci:(ci + 3)]),
        if(ri <= rm - 3) f(d[ri:(ri + 3), ci]),
        if (ci <= cm - 3 && ri <= rm - 3)
          f(c(d[ri, ci], d[ri + 1, ci + 1], d[ri + 2, ci + 2], d[ri + 3, ci + 3])),
        if (ci <= cm - 3 && ri > 3)
          f(c(d[ri, ci], d[ri - 1, ci + 1], d[ri - 2, ci + 2], d[ri - 3, ci + 3]))
      ))
  }
}
n

# Part 2
g <- \(s) paste0(s, collapse = "") %in% c("MAS", "SAM")
n <- 0

for (ri in 2:(rm - 1)) {
  for (ci in 2:(cm - 1)) {
    n <- 
    n + 
      (g(c(d[ri - 1, ci - 1], d[ri, ci], d[ri + 1, ci + 1])) &&
       g(c(d[ri + 1, ci - 1], d[ri, ci], d[ri - 1, ci + 1]))
      )
  }
}
n