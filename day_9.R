# AoC 2024 - Day 9

# Part 1
d <- as.numeric(unlist(strsplit(readLines("input.txt"), "")))
g <- 1:length(d)
f <- g * (g %% 2)
h <- 0:(length(d[f]) - 1)
r <- rev(rep(h, d[f]))[1:(sum(d[!f]) - 1)]
x <- data.frame(h, d[f], c(d[!f], 0))
y <- unlist(apply(x, 1, \(x) c(rep(x[1], x[2]), rep(".", x[3]))))
z <- as.numeric(replace(y, which(y == "."), r))[1:sum(d[f])]
sum(0:(length(z) - 1) * z)