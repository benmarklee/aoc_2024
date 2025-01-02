
# AoC 2024 - Day 3

# Part 1
x <- Reduce(paste0, readLines("input.txt"))
f <- \(x) sum(unlist(sapply(regmatches(x, gregexpr("mul\\((\\d+),(\\d+)\\)", x)),\(x) {
  lapply(strsplit(gsub("\\)", "", gsub("mul\\(", "", x)), ","), \(x) prod(as.numeric(x)))
})))
f(x)

# Part 2
z <- unlist(strsplit(x, "don't\\(\\)"))
f(c(z[[1]][1], unlist(lapply(strsplit(z, "do\\(\\)"), \(x) x[-1]))))
