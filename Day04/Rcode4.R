d4 <- readLines("Day04/input4")
d4_s <- strsplit(d4[1], ",")[[1]]
d4_b <- d4[3:length(d4)]

start <- seq(1, length(d4_b), 6)
end <- c(seq(5, length(d4_b), 6), length(d4_b))

boards <- list()

for (idx in seq_along(start)) {
  boards[[idx]] <- do.call(rbind, strsplit(trimws(d4_b[start[idx]:end[idx]]), "[ ]{1,}"))
}

boards2 <- boards

wins <- NULL
for (num in d4_s) {
  for (idx in seq_along(boards2)) {
    boards2[[idx]][boards2[[idx]] == num] <- ""
    r <- apply(boards2[[idx]] == "", 1, all)
    rw <- which(r)
    c <- apply(boards2[[idx]] == "", 2, all)
    cw <- which(c)
    if (any(r)) {
      browser()
      wins <- c(wins, sum(as.numeric(boards2[[idx]]), na.rm = TRUE) * as.numeric(num))
    }
    if (any(c)) {
      wins <- c(wins, sum(as.numeric(boards2[[idx]]), na.rm = TRUE) * as.numeric(num))
    }
  }
  if (length(wins)) {
    break
  }
}

#1
wins

boards2 <- boards

wins <- NULL
boards_taken <- seq_along(boards2)
for (num in d4_s) {
  for (idx in boards_taken) {
    boards2[[idx]][boards2[[idx]] == num] <- ""
    r <- apply(boards2[[idx]] == "", 1, all)
    c <- apply(boards2[[idx]] == "", 2, all)
    if (any(r)) {
      wins <- c(wins, sum(as.numeric(boards2[[idx]]), na.rm = TRUE) * as.numeric(num))
      boards_taken <- boards_taken[boards_taken != idx]
    }
    if (any(c)) {
      wins <- c(wins, sum(as.numeric(boards2[[idx]]), na.rm = TRUE) * as.numeric(num))
      boards_taken <- boards_taken[boards_taken != idx]
    }
  }
}
# 2
wins[length(wins)]
