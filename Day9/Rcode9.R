d9 <- readLines("Day9/input9")

d9_mat_raw <- do.call(rbind, strsplit(d9, ""))
class(d9_mat_raw) <- "numeric"

d9_mat <- OpenImageR::padding(d9_mat_raw, nrow(d9_mat_raw) + 4, ncol(d9_mat_raw) + 4, fill_value = NA)$data

conv_mat <- matrix(c(NA,1,NA,1,NA,1,NA,1,NA), ncol = 3)

mins <- NULL
for (r in 1:(nrow(d9_mat) - 2)) {
  for (col in  1:(ncol(d9_mat) - 2)) {
      cc <- d9_mat[r:(r+2), col:(col+2)]
      cc1 <- as.numeric(na.omit(as.numeric(cc * conv_mat)))
      cc2 <- cc[2, 2]
      if (is.na(cc2)) next
      if (all(cc1 > cc2)) {
        mins <- c(mins, cc2)
      }
  }
}

sum(mins + 1)

d9_mat_raw[d9_mat_raw == 9] <-  NA

d9_mat_raw
