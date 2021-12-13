d13 <- readLines("Day13/input13")
which_empty <- which(d13 == "")
d13_c <- do.call(rbind, strsplit(d13[1:(which_empty - 1)], ","))
class(d13_c) <- "numeric"
d13_w <- do.call(rbind, strsplit(d13[(which_empty + 1):length(d13)], "=|along "))[, 2:3]

maxs <- apply(d13_c, 2, max)
nrow_max <- maxs[2] + 1
ncol_max <- maxs[1] + 1
mm <- matrix(0, nrow_max, ncol_max)
for (i in seq_len(nrow(d13_c))) {
  mm[d13_c[i, 2] + 1, d13_c[i, 1] + 1] <- 1
}

flip_y <- function(x, at) {
  res <- x[1:(at - 1), ] + x[nrow(x):(at + 1), ]
  res[res > 0] <- 1
  res
}

flip_x <- function(x, at) {
  res <- x[, 1:(at - 1)] + x[, ncol(x):(at + 1)]
  res[res > 0] <- 1
  res
}

mat <- mm
for (r in seq_len(nrow(d13_w))) {
  if (d13_w[r,1] == "x") {
    mat <- flip_x(mat, as.integer(d13_w[r,2]) + 1)
  } else {
    mat <- flip_y(mat, as.integer(d13_w[r,2]) + 1)
  }
}

mat
