d11 <- readLines("Day11/input11")
d11_o <- do.call(rbind, strsplit(d11, ""))
class(d11_o) <- "numeric"
library(collections)
library(hash)

up <- function(x) c(max(x[1] - 1, 1), x[2])
down <- function(x, nrow_max) c(min(x[1] + 1, nrow_max), x[2])
left <- function(x) c(x[1], max(x[2] - 1, 1))
leftup <- function(x) c(max(x[1] - 1, 1), max(x[2] - 1, 1))
leftdown <- function(x, nrow_max) c(min(x[1] + 1, nrow_max), max(x[2] - 1, 1))
right <- function(x, ncol_max) c(x[1], min(x[2] + 1, ncol_max))
rightup <- function(x, ncol_max) c(max(x[1] - 1, 1), min(x[2] + 1, ncol_max))
rightdown <- function(x, nrow_max, ncol_max) c(min(x[1] + 1, nrow_max), min(x[2] + 1, ncol_max))

through_9points <- function(mat, nrow_max, ncol_max, flash, all_9, all_9_iter, deque9) {
  while (isTRUE(deque9$size() > 0)) {
    new_c <- deque9$pop()
    l <- left(new_c)
    lu <- leftup(new_c)
    ld <- leftdown(new_c, nrow_max)
    r <- right(new_c, ncol_max)
    ru <- rightup(new_c, ncol_max)
    rd <- rightdown(new_c, nrow_max, ncol_max)
    u <- up(new_c)
    d <- down(new_c, nrow_max)
    for (i in unique(list(l, lu, ld, ru, rd, r, u, d))) {
      val_temp <- mat[i[1], i[2]]
      indx <- paste0(i[1], ":", i[2])
      if (val_temp <= 0) {
      } else if (val_temp >= (flash)) {
        mat[i[1], i[2]] <- 0
        if ((!indx %in% hash::keys(all_9_iter))) {
          deque9$push(i)
          all_9[[indx]] <- if (is.null(all_9[[indx]])) 1 else all_9[[indx]] + 1
          all_9_iter[[indx]] <- NA
        }
      } else {
        mat[i[1], i[2]] <- val_temp + 1
      }
    }
  }
  list(mat = mat, deque9 = deque9, all_9 = all_9, all_9_iter = all_9_iter)
}

through_new9points <- function(mat, nrow_max, ncol_max, flash, all_9, all_9_iter, deque9) {
  ww <- which(mat == (flash))
  poss_col <- ceiling(ww / nrow_max)
  poss_row <- ww %% nrow_max
  poss_row[poss_row == 0] <- nrow_max
  for (ix in seq_along(poss_col)) {
    pos_f <- paste0(poss_row[ix], ":", poss_col[ix])
    all_9[[pos_f]] <- if (is.null(all_9[[pos_f]])) 1 else all_9[[pos_f]] + 1
    all_9_iter[[pos_f]] <- NA
    deque9$push(c(poss_row[ix], poss_col[ix]))
    mat[poss_row[ix], poss_col[ix]] <- NA
  }
  list(mat = mat, deque9 = deque9, all_9 = all_9, all_9_iter = all_9_iter)
}

floodFlashFun1 <- function(mat, flash = 9L, iters = 1L) {
  stopifnot(is.matrix(mat) && is.integer(flash) && is.integer(iters))
  nrow_max <- nrow(mat)
  ncol_max <- ncol(mat)
  iter <- 0
  all_9 <- hash::hash()
  all_9_iter <- hash::hash()
  deque9 <- collections::deque()

  while (iter <= (iters)) {

    through_res <- through_9points(mat, nrow_max, ncol_max, flash, all_9, all_9_iter, deque9)
    mat <- through_res$mat
    deque9 <- through_res$deque9
    all_9 <- through_res$all_9
    all_9_iter <- through_res$all_9_iter

    if (iter == iters) return(all_9)

    deque9 <- collections::deque()
    all_9_iter <- hash::hash()

    if (any(mat == flash)) {
      through_res <- through_new9points(mat, nrow_max, ncol_max, flash, all_9, all_9_iter, deque9)
      mat <- through_res$mat
      deque9 <- through_res$deque9
      all_9 <- through_res$all_9
      all_9_iter <- through_res$all_9_iter
    }
    mat <- mat + 1
    mat[is.na(mat)] <- 0
    iter <- iter + 1
  }
}

#1
ff <- floodFlashFun1(d11_o, 9L, 100L)
sum(hash::values(ff))

floodFlashFun2 <- function(mat, flash = 9) {
  stopifnot(is.matrix(mat) && is.integer(flash))
  nrow_max <- nrow(mat)
  ncol_max <- ncol(mat)
  iter <- 0
  all_9 <- hash::hash()
  all_9_iter <- hash::hash()
  deque9 <- collections::deque()

  while (TRUE) {
    through_res <- through_9points(mat, nrow_max, ncol_max, flash, all_9, all_9_iter, deque9)
    mat <- through_res$mat
    deque9 <- through_res$deque9
    all_9 <- through_res$all_9
    all_9_iter <- through_res$all_9_iter

    if (all(as.logical(mat == 0))) return(iter)

    deque9 <- deque()
    all_9_iter <- hash::hash()

    if (any(mat == flash)) {
      through_res <- through_new9points(mat, nrow_max, ncol_max, flash, all_9, all_9_iter, deque9)
      mat <- through_res$mat
      deque9 <- through_res$deque9
      all_9 <- through_res$all_9
      all_9_iter <- through_res$all_9_iter
    }
    mat <- mat + 1
    mat[is.na(mat)] <- 0
    iter <- iter + 1
  }
}

#2
floodFlashFun2(d11_o, 9L)
