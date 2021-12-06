d2 <- readLines("Day2/input2")
d2d <- vapply(strsplit(d2, " "), function(x) x[1], character(1))
d2n <- as.numeric(vapply(strsplit(d2, " "), function(x) x[2], character(1)))

fn <- d2n[grep("forward", d2)]
dn <- d2n[grep("down", d2)]
un <- d2n[grep("up", d2)]

#1
sum(fn) * abs(sum(un) - sum(dn))

res <- list(aim = 0, forward = 0, depth = 0)

for (i in seq_along(d2)) {
  if (d2d[i] == "down") {
    res[["aim"]] <- res[["aim"]] + d2n[i]
  } else if (d2d[i] == "up") {
    res[["aim"]] <- res[["aim"]] - d2n[i]
  } else if (d2d[i] == "forward") {
    res[["forward"]] <- res[["forward"]] + d2n[i]
    res[["depth"]] <- res[["depth"]] + res[["aim"]] * d2n[i]
  }
}
#2
res$forward * res$depth
