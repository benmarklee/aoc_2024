# Day 8 - Part 1
d <- readLines("input.txt")
m <- matrix(unlist(strsplit(d, "")), nr = length(d), by = T)
mr <- nrow(m)
mc <- ncol(m)
r <- matrix(0, mr, mc)
u <- unique(as.vector(m))
u <- u[u != "."]

lapply(u, \(x) {
  h <- matrix(which(m == x, arr.ind = T), nc = 2)
  n <- nrow(h)
  for (i in 1:n) {
    for (j in i:n) {
      if (sum(h[i,] == h[j,]) < 2) {
        x1 <- h[i,1]
        y1 <- h[i,2]
        x2 <- h[j,1]
        y2 <- h[j,2]
        rd <- x2 - x1
        cd <- y2 - y1
        xa <- x1 - rd
        ya <- y1 - cd
        xb <- x2 + rd
        yb <- y2 + cd
        if (ya %in% 1:mr && xa %in% 1:mc) r[ya, xa] <<- 1
        if (yb %in% 1:mr && xb %in% 1:mc) r[yb, xb] <<- 1
      }
    }
  }
})
sum(r)

# Part 2
r <- matrix(0, mr, mc)
r[t(m) != "."] <- 1

lapply(u, \(x) {
  h <- matrix(which(m == x, arr.ind = T), nc = 2)
  n <- nrow(h)
  for (i in 1:n) {
    for (j in i:n) {
      if (sum(h[i,] == h[j,]) < 2) {
        x1 <- h[i,1]
        y1 <- h[i,2]
        x2 <- h[j,1]
        y2 <- h[j,2]
        rd <- x2 - x1
        cd <- y2 - y1
        xa <- x1 - rd
        ya <- y1 - cd
        xb <- x2 + rd
        yb <- y2 + cd
        while (ya %in% 1:mr && xa %in% 1:mc) {
          r[ya, xa] <<- 1
          xa <- xa - rd
          ya <- ya - cd
        }
        while (yb %in% 1:mr && xb %in% 1:mc) {
          r[yb, xb] <<- 1
          xb <- xb + rd
          yb <- yb + cd
        }
      }
    }
  }
})
sum(r)