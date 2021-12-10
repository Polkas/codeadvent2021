d6 <- readLines("Day06/input6")
d6_f <- as.numeric(strsplit(d6, ",")[[1]])

fish_repro <- function(vec, res = NULL, iter = 1, stop = 80) {
  vec <- vec - 1
  vec <- c(vec, rep(8, sum(vec == -1)))
  vec[vec == -1] <- 6
  res <- c(res, length(vec))
  if (iter == (stop)) {
    return(res)
  } else {
    fish_repro(vec, res, iter = iter + 1, stop = stop)
  }
}

# 1
res <- fish_repro(d6_f, stop = 80)
res[length(res)]

fish_repro_improved <- function(res = NULL, iter = 1, stop = 80) {
  res0 <-  res["0"]
  res["0"] <- res["1"]
  res["1"] <- res["2"]
  res["2"] <- res["3"]
  res["3"] <- res["4"]
  res["4"] <- res["5"]
  res["5"] <- res["6"]
  res["6"] <-  res["7"] + res0
  res["7"] <-  res["8"]
  res["8"] <- res0
  if (iter == (stop)) {
    return(res)
  } else {
    fish_repro_improved(res, iter = iter + 1, stop = stop)
  }
}

bb <- setNames(rep(0, 9), 0:8)
tt <- table(d6_f)
for (i in names(tt)) bb[i] <- tt[i]

options(scipen = 999)

# 2
res <- fish_repro_improved(bb, stop = 256)
sum(res)
