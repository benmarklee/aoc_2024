# AoC 2024 day 6

# Part 1
m <- readLines("input.txt")
m <- matrix(unlist(strsplit(m, "")), nc = length(m), by = T)
p <- s <- which(m == "^", arr.ind = T)
n <- ncol(m)
b <- matrix(0, nrow(m), n)
b[p] <- i <-  1
try({while ((p + d[[i]])[1] %in% 1:n && (p + d[[i]])[2] %in% 1:n)
  for (i in 1:4)
    while (m[p + d[[i]]] != "#") {
      p <- p + d[[i]]
      b[p] <- 1
}})
sum(b)

# Part 2
z <- 0
for (h in 1:n) {
  for (j in 1:n) {
    p <- s
    w <- m
    if (sum(s == c(h,j)) < 2) w[h,j] <- "#"
    b <- matrix(0, nrow(m), n)
    b[p] <- i <- 1
    e <- 0
    try((while ((p + d[[i]])[1] %in% 1:n && (p + d[[i]])[2] %in% 1:n && e < 1) {
      for (i in 1:4) {
        while (w[p + d[[i]]] != "#" && e < 1) {
          p <- p + d[[i]]
          if (b[p] > 4) e <- 1 else b[p] <- b[p] + 1
        }
        if (e > 0) {
          z <- z + 1
          break
        }
      }
    }))
  }
}
z


