library(dplyr)
library(tidyr)
library(collections)

d5 <- readLines("Day05/input5")

d5_df <- tidyr::separate(data.frame(d5), "d5", into = c("x1", "y1", "x2", "y2")) %>%
  mutate_all(as.numeric)

# not RAM efficient
#1
d5_df %>%
  filter(y1 == y2 | x1 == x2) %>%
  rowwise() %>%
  mutate(
    points = list(data.frame(
      point_x = as.integer(seq(x1, x2)),
      point_y = as.integer(seq(y1, y2))
    ))
  ) %$% do.call(rbind, points) %>%
  group_by(point_x, point_y) %>%
  summarise(n = n(), .groups = "keep") %>%
  filter(n >= 2)  %>% nrow()



gridvh <- hash::hash()
for (r in seq_len(nrow(d5_df))) {
  x1 <- d5_df[r, 1]
  y1 <- d5_df[r, 2]
  x2 <- d5_df[r, 3]
  y2 <- d5_df[r, 4]

  if (x1 == x2 || y1 == y2) {
    points = paste0(
      point_x = as.integer(seq(x1, x2)),
      ":",
      point_y = as.integer(seq(y1, y2))
    )
  } else {
    lms = coef(lm.fit(cbind(1, c(x1, x2)), c(y1, y2)))
    inter = lms[1]
    coef = lms[2]
    points = paste0(
      point_x = as.integer(seq(x1, x2)),
      ":",
      point_y = as.integer(round(inter + seq(x1, x2) * coef))
    )
  }
  for (p in points) {
    gridvh[[p]] <- if (length(gridvh[[p]]) == 0) 1 else  gridvh[[p]] + 1
  }
}
sum(hash::values(gridvh) >= 2)
