d10 <- readLines("Day10/input10")
d10_c <- strsplit(d10, "")

library(collections)

find_first_wrong_bracket <- function(x) {
  stopifnot(is.list(x))

  bracs <- data.frame(
    left = c("[", "(", "{", "<"),
    right = c("]", ")", "}", ">")
  )

  deque <- deque()
  iter <- 0
  res <- NULL
  for (i in d10_c) {
    for (e in i) {
      if (e %in% bracs$left) {
        deque$pushleft(e)
      } else {
        if (!identical(deque$popleft(),  bracs$left[match(e, bracs$right)])) {
          res <- c(res, e)
          break
        }
      }
    }
    iter <- iter + 1
  }
  res
}

points_b <- data.frame(
  poits = c(3, 57, 1197, 25137),
  b = c(")", "]", "}", ">")
)

sum(points_b$poits[match(find_first_wrong_bracket(d10_c), points_b$b)])
