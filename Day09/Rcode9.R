#install.packages("OpenImageR")
#install.packages("BiocManager")
#BiocManager::install("EBImage")
d9 <- readLines("Day9/input9")

d9_mat_raw <- do.call(rbind, strsplit(d9, ""))
class(d9_mat_raw) <- "numeric"

low_points <- function(mat) {
  stopifnot(is.matrix(mat))
  d9_mat <- OpenImageR::padding(d9_mat_raw,
                                nrow(d9_mat_raw) + 4,
                                ncol(d9_mat_raw) + 4, fill_value = NA)$data
  conv_mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA), ncol = 3)
  mins <- NULL
  for (r in 1:(nrow(d9_mat) - 2)) {
    for (col in  1:(ncol(d9_mat) - 2)) {
      cc <- d9_mat[r:(r + 2), col:(col + 2)]
      cc1 <- as.numeric(na.omit(as.numeric(cc * conv_mat)))
      cc2 <- cc[2, 2]
      if (is.na(cc2)) next
      if (all(cc1 > cc2)) {
        mins <- c(mins, cc2)
      }
    }
  }
  mins
}
#1
sum(low_points(d9_mat_raw) + 1)

flood_n_pergroup <- function(mat) {
  stopifnot(is.matrix(mat))
  stopifnot(all(mat %in% c(0,1)))
  n_row <- nrow(mat)
  maxs <- NULL
  while (any(mat != 1)) {
    vals <- which(mat != 1)
    ss <- sample(length(vals), 1)
    col <- (ceiling(vals / n_row))[1]
    r <- (vals %% n_row)[1]
    if (r == 0) {
      r <- n_row
    }
    mat <- EBImage::floodFill(mat, c(r, col), 0.5)
    maxs <- c(maxs, sum(mat == 0.5))
    mat[mat == 0.5] <- 1
  }
  maxs
}

d9_mat_2 <- d9_mat_raw
d9_mat_2[d9_mat_2 != 9] <- 0
d9_mat_2[d9_mat_2 == 9] <- 1
maxs <- flood_n_pergroup(d9_mat_2)
#2
prod(sort(maxs, decreasing = TRUE)[1:3])
