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
  res <- NULL
  for (i in x) {
    res_val <- NA
    for (e in i) {
      if (e %in% bracs$left) {
        deque$push(e)
      } else {
        if (!identical(deque$pop(),  bracs$left[match(e, bracs$right)])) {
          res_val <- e
          break
        }
      }
    }
    res <- c(res, res_val)
  }
  res
}

points_b <- data.frame(
  poits = c(3, 57, 1197, 25137),
  b = c(")", "]", "}", ">")
)

fb <- find_first_wrong_bracket(d10_c)

sum(points_b$poits[match(fb, points_b$b)], na.rm = TRUE)

to_fill <- which(is.na(fb))

d10_c2 <- d10_c[to_fill]

find_lack_brackets <- function(x) {
  stopifnot(is.list(x))

  bracs <- data.frame(
    left = c("[", "(", "{", "<"),
    right = c("]", ")", "}", ">")
  )

  res <- list()
  for (i in x) {
    deque <- deque()
    for (e in i) {
      if (e %in% bracs$left) {
        deque$push(e)
      } else {
        deque$pop()
      }
    }
    res <- append(res, list(rev(bracs$right[match(unlist(deque$as_list()), bracs$left)])))
  }
  res
}

bb <- find_lack_brackets(d10_c2)

points_b2 <- data.frame(
  points = c(1, 2, 3, 4),
  b = c(")", "]", "}", ">")
)

# a + (5*a+b) + ((5*a+b)*5 + c) + (((5*a+b)*5 + c)*5 + d)
# 2
res <- lapply(bb, function(x) {
  res <- points_b2$points[match(x, points_b2$b)]
  len <- length(res)
  sum(vapply(seq_len(len), function(x) res[x]*(5**(len - x)), numeric(1)))
})
median(unlist(res))
