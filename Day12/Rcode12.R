d12 <- readLines("Day12/input12")

library(igraph)
library(collections)

d12_df <- do.call(rbind, strsplit(d12, "-"))
#gg <- graph_from_edgelist(d11_df, directed <- FALSE)
#igraph::plot.igraph(gg)

d12_1 <- tapply(d11_df[, 2], d12_df[, 1], list)
d12_2 <- tapply(d11_df[, 1], d12_df[, 2], list)
binded <- c(d12_1, d12_2)
adjlist <- tapply(binded, names(binded), function(x) unique(unlist(x, FALSE, FALSE)))

# is_lowercase <- function(x) x == tolower(x)
is_uppercase <- function(x) x == toupper(x)

rec_caves <- function(pos, from, to, caves, visited, allow_revisit_small) {
  count <- 0
  if (pos == to) {
    return(1)
  }
  visited$push(pos)
  for (cave in caves[[pos]]) {
    if (cave == from) {
      next
    }

    is_big <- is_uppercase(cave)
    visited_before <- cave %in% visited$as_list()
    revisited_small <- (!is_big) && visited_before

    # new one, big one or small one second visit
    allow_visit <- (!visited_before) || is_big || allow_revisit_small
    # next one blocked
    revisit_small <- allow_revisit_small && (!revisited_small)
    if (allow_visit) count <- count + rec_caves(cave, from, to, caves, visited, revisit_small)
  }
  visited$pop()
  return(count)
}

#1
rec_caves("start", "start", "end", adjlist, collections::stack(), FALSE)
#2
rec_caves("start", "start", "end", adjlist, collections::stack(), TRUE)
