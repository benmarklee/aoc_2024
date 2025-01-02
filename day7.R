# AoC 2024 - Day 7
 
# Part 1
d <- readLines("input.txt")[11:120]

sum(sapply(d, \(d) {
  
w <- unlist(strsplit(d, ": "))
r <- as.numeric(w[1])
x <- as.numeric(unlist(strsplit(w[-1], " ")))
le <- length(x) - 1
op <- lapply(1:2^le, \(x) as.numeric(intToBits(x))[1:le])
f <- \(x, op) {
  interm <- c(if (op[1]) x[1] + x[2] else x[1] * x[2], x[-(1:2)])
  if (length(op) == 1) return(interm) else f(interm, op[-1])
}

  (sum(sapply(op, \(op) f(x, op) == r)) > 0) * r
})) -> final

# Part 2

base3 <- \(x, y = "") {
  y <- paste0(y, x %% 3)
  z <- x %/% 3
  if (z == 0) return(as.numeric(strsplit(y, "")[[1]])) else base3(z, y)
}

d <- readLines("input.txt")
sum(sapply(d, \(d) {
  
  w <- unlist(strsplit(d, ": "))
  r <- as.double(w[1])
  cat(r)
  cat("\n")
  x <- as.numeric(unlist(strsplit(w[-1], " ")))
  le <- length(x) - 1
  op <- lapply(0:(3^le - 1), \(x) {
    b3 <- base3(x)
    if (length(b3) < le) c(b3, rep(0, le - length(b3))) else b3
  })
  
  f <- \(x, op) {
    interm <- c(
      if (op[1] == 0) {
        x[1] + x[2]
      } else if (op[1] == 1) {
        x[1] * x[2]
      } else {
        as.numeric(paste0(x[1], x[2]))   
      },
      x[-(1:2)]
    )
    if (length(op) == 1) return(interm) else f(interm, op[-1])
  }
  
  e <- 0
  for (i in 1:length(op)) {
    te <- f(x, op[[i]])
    # cat(te)
    if(!is.na(te) && te == r) {
      # cat(r)
      e <- 1
      break
    }
  }
  r * e
})) -> final
final
