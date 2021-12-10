d3 <- readLines("Day03/input3")
d3_mat <- do.call(rbind, strsplit(d3, ""))
res <- apply(d3_mat, 2, function(x) names(sort(table(x)))[2])
gamma = strtoi(paste0(res, collapse = ""), base = 2)
epilion = strtoi(paste0(as.integer(!as.numeric(res)), collapse = ""), base = 2)

# 1
gamma * epilion

most_common <- function(data) {
  res_f <- ""
  dat <- data
  for(i in seq_len(ncol(data))) {
    tt <- table(dat[, i, drop = FALSE])
    if (tt[1] == tt[2]) {
      val <- "1"
    } else {
      val <- names(sort(tt, decreasing = TRUE))[1]
    }
    res_f <- paste0(res_f, val, collapse = "")
    dat <- dat[dat[, i] == val, , drop = FALSE]
    if (isTRUE(nrow(dat) == 0) || length(dat) == 0) break
  }
  res_f
}

least_common <- function(data) {
  res_f <- ""
  dat <- data
  for(column in seq_len(ncol(data))) {
    tt <- table(dat[, column, drop = FALSE])
    if (isTRUE(tt[1] == tt[2])) {
      val <- "0"
    } else {
      val <- names(sort(tt))[1]
    }
    res_f <- paste0(res_f, val, collapse = "")
    dat <- dat[dat[, column] == val, , drop = FALSE]
    if (isTRUE(nrow(dat) == 0) || length(dat) == 0) break
  }
  res_f
}

most_common2 <- function(dat, res = "") {
  tt <- table(dat[, 1, drop = FALSE])
  if (isTRUE(tt[1] == tt[2])) {
    val <- "1"
  } else {
    val <- names(sort(tt, decreasing = TRUE))[1]
  }
  dat <- dat[dat[, 1] == val, , drop = FALSE]
  res_f <- paste0(res, val, collapse = "")
  if (isTRUE(nrow(dat) == 0 || length(dat) == 0 || ncol(dat) == 1)) return(res_f)
  most_common2(dat[, -1, drop = FALSE], res_f)
}

least_common2 <- function(dat, res = "") {
  tt <- table(dat[, 1, drop = FALSE])
  if (isTRUE(tt[1] == tt[2])) {
    val <- "0"
  } else {
    val <- names(sort(tt))[1]
  }
  dat <- dat[dat[, 1] == val, , drop = FALSE]
  res_f <- paste0(res, val, collapse = "")
  if (isTRUE(nrow(dat) == 0 || length(dat) == 0 || ncol(dat) == 1)) return(res_f)
  least_common2(dat[, -1, drop = FALSE], res_f)
}

#2
strtoi(most_common(d3_mat), base = 2) * strtoi(least_common(d3_mat), base = 2)
strtoi(most_common2(d3_mat), base = 2) * strtoi(least_common2(d3_mat), base = 2)
