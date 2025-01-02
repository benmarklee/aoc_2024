d <- readLines("input.txt")
# d <- "0 0"
d <- "0"
d
tot <- list(d)

for (i in 1:25) {
  d <- unlist(strsplit(d, " "))
  d <- lapply(d, \(x) {
    if (x == "0") {
      1
    } else if (nchar(x) %% 2 == 0) {
      list(
        as.numeric(paste0(unlist(strsplit(x, ""))[1:(nchar(x) / 2)], collapse = "")),
        as.numeric(paste0(unlist(strsplit(x, ""))[((nchar(x) / 2) + 1):nchar(x)], collapse = ""))
      )
    } else {
      as.numeric(x) * 2024
    }
  }) |> unlist() |> paste0(collapse = " ")
  tot <- c(tot, d)
}
length(unlist(strsplit(d, " ")))
d
tot

f(d, 25) # Part 1
f(d, 75) # Part 2

12345678

1234 5678

12 34 56 78

1 2 3 4 5 6 7 8

2024 4048

