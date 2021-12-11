d8 <- readLines("Day08/input8")
library(dplyr)
output_cols <- paste0("output_", 1:4)
input_cols <- paste0("input_", 1:10)

d8_f <- tidyr::separate(
  data.frame(d8),
  col = "d8",
  into = c(input_cols, output_cols)
  )

d8_m <- as.matrix(d8_f)
#1
sum(nchar(as.matrix(d8_m[, output_cols])) %in% c(2:4, 7))

library(sets)
inputs <- d8_m[, input_cols]
which_known <- matrix(nchar(t(inputs)) %in% c(2,3,4,7), nrow = 10)
inputs_known <- t(matrix(t(inputs)[which_known], nrow = 4))
inputs_known <- t(apply(inputs_known, 1, function(x) x[order(nchar(x))]))
inputs_unknown <- t(matrix(t(inputs)[!which_known], nrow = 6))
inputs_unknown <- t(apply(inputs_unknown, 1, function(x) x[order(nchar(x))]))

res <- list()
for (row in seq_len(nrow(inputs_known))) {
  ii <- hash::hash()
  ss <- strsplit(inputs_known[row, ], "")
  ii[["1"]] <- sets::as.set(ss[[1]])
  ii[["7"]] <- sets::as.set(ss[[2]])
  ii[["4"]] <- sets::as.set(ss[[3]])
  ii[["8"]] <- sets::as.set(ss[[4]])
  res[[row]] <- ii
}

for (row2 in seq_len(nrow(inputs_unknown))) {
  ii2 <- res[[row2]]
  ss <- strsplit(inputs_unknown[row2, ], "")
  for (i in 1:3) {
    sss <- sets::as.set(ss[[i]])
    if (ii2[["1"]] <= sss) {
      ii2[["3"]] <- sss
    } else if ((sss | ii2[["4"]]) == ii2[["8"]]) {
      ii2[["2"]] <- sss
    } else {
      ii2[["5"]] <- sss
    }
  }
  for (i in 4:6) {
    sss <- sets::as.set(ss[[i]])
    if (!ii2[["1"]] <= sss) {
      ii2[["6"]] <- sss
    } else if (!ii2[["4"]] <= sss) {
      ii2[["0"]] <- sss
    } else {
      ii2[["9"]] <- sss
    }
  }
  res[[row2]] <- ii2
}

res2 <- list()
output <- d8_m[, output_cols]
for (row in seq_len(nrow(output))) {
  ii <- ""
  for (col in seq_len(ncol(output))){
    current_set <- strsplit(output[row, col], "")[[1]]
    rr <- res[[row]]
    ww <- which(vapply(hash::values(rr), function(x) sets::as.set(current_set) == x, logical(1)))
    val <- hash::keys(rr)[ww]
    ii <- paste0(ii, val)
  }
  res2[[row]] <- ii
}

sum(as.numeric(unlist(res2)))
#1068933
