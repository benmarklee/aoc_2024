
# AoC 2024 - Day 5

d <- readLines("input.txt")
r <- d[grepl("\\|", d)]
i <- matrix(as.numeric(sapply(strsplit(r, "\\|"), rev)), nc = 2, b = T)
s <- matrix(1, 99, 99); s[i] <- 0
u <- d[grepl(",", d)]
f <- \(x) as.numeric(unlist(strsplit(x, ",")))
k <- sapply(u, \(x) { v <- f(x); prod(s[cbind(v[-length(v)], v[-1])])})
l <- 1:length(k)
g <- \(x) (length(x) + 1) / 2

# Part 1
sum(sapply(u[k * l], \(x) { y <- f(x); y[g(y)]}))

# Part 2
sum(sapply(sapply(u[!k * l], f), \(x) x[colSums(s[x,x]) == g(x)]))
