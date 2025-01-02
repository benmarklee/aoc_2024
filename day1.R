
# AoC 2024 - Day 1

x <- read.table("input.txt")

sum(abs(sort(x$V1) - sort(x$V2)))              # Part 1

sum(x$V1 * rowSums(outer(x$V1, x$V2, "==")))   # Part 2
