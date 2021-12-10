d8 <- readLines("Day8/input8")
library(dplyr)
output_cols <- paste0("output_", 1:4)
input_cols <- paste0("input_", 1:10)

d8_f <- tidyr::separate(
  data.frame(d8),
  col = "d8",
  into = c(input_cols, output_cols)
  )

d8_m <- as.matrix(d8_f)

sum(nchar(as.matrix(d8_m[, output_cols])) %in% c(2:4, 7))
