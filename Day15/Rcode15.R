library(igraph)
library(data.table)
d15 <- readLines("Day15/input15")
d15_mat <- do.call(rbind, strsplit(d15, ""))
class(d15_mat) <- "numeric"

if_healthy <- function(x, rc_max) {
  if (any(x > rc_max) || any(x < 1)) {
    NULL
  } else {
    x
  }
}

to_fullc <- function(x, c_max) {
  if (x[2] == 1) x[1] else x[1] + (x[2] - 1) * c_max
}

get_neibo <- function(r, c, r_max, c_max) {
  raw <- Filter(Negate(is.null),
                Map(if_healthy,
                    list(c(r, c + 1),
                         c(r + 1, c),
                         c(r - 1,  c),
                         c(r, c - 1)), r_max))
  new <- vapply(raw, function(x) to_fullc(x, c_max), numeric(1))
  new
}

get_mat_graph <- function(mat) {
  nn <- nrow(mat)
  rr <- nrow(mat)
  res <- vector("list", nn * rr)
  iter <- 1
  for (r in seq_len(rr)) {
    for (c in seq_len(nn)) {
      neibos <- get_neibo(r, c, rr, nn)
      base <- if (c == 1) r else r + (c - 1) * nn
      res[[iter]] <- cbind(rep(base, length(neibos)), neibos, mat[neibos])
      iter <- iter + 1
    }
  }
  do.call(rbind, res)
}

d15_mat_graph <- get_mat_graph(d15_mat)

ig <- graph_from_edgelist(as.matrix(d15_mat_graph[ ,1:2]))
E(ig)$weight <- d15_mat_graph[,3]
pp = igraph::shortest_paths(ig, 1, max(d15_mat_graph[,2]), output = "epath")
# 1
sum(E(ig)$weight[pp$epath[[1]]])

#####################################

res <- list()
for (i in seq_len(nrow(d15_mat) * 5)) {
  row <- i %% nrow(d15_mat)
  if (row == 0) row <- nrow(d15_mat)
  i <- ceiling(i / nrow(d15_mat))
  res <- c(res, list((rep(d15_mat[row,], 5) + rep((i - 1):(i + 3), each = nrow(d15_mat)))))
}

mat_raw <- do.call(rbind, res) %% 9
mat_raw[mat_raw == 0] <- 9
d15_mat_graph <- get_mat_graph(mat_raw)
ig <- graph_from_edgelist(as.matrix(d15_mat_graph[ , 1:2]))
E(ig)$weight <- d15_mat_graph[,3]
pp <- igraph::shortest_paths(ig, 1, max(d15_mat_graph[,2]), output = "epath")
# 2
sum(E(ig)$weight[pp$epath[[1]]])
