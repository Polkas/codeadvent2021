d14 <- readLines("Day14/input14")
d14_p <- strsplit(d14, " -> ")
d14_start <- d14_p[[1]]
d14_trans <- as.data.frame(do.call(rbind, d14_p[2:length(d14_p)]))

impute_polymer_improved <- function(string_start, transitions, iters = 40) {
  stopifnot(is.integer(iters))
  stopifnot(ncol(transitions) == 2)
  stopifnot(is.character(string_start) && (length(string_start) == 1))

  split_char_by2 <- function(x) {
    zoo::rollapply(strsplit(x, "")[[1]],
                   FUN = function(x) paste(x, collapse = ""),
                   width = 2)
  }

  if_null <- function(x, y) if (is.null(x)) y else x

  transitions_temp <- do.call(rbind, strsplit(transitions[, 1], ""))

  transitions[["new1"]] <- paste0(transitions_temp[, 1], transitions[, 2])
  transitions[["new2"]] <- paste0(transitions[, 2], transitions_temp[, 2])

  rules1 <- setNames(transitions[, 2], transitions[, 1])
  rules2 <- setNames(transitions[, 3], transitions[, 1])
  rules3 <- setNames(transitions[, 4], transitions[, 1])

  element_counts <- hash::hash()
  pair_counts <- hash::hash()
  for (e in d14_trans[, 1])
    pair_counts[[e]] <- 0
  pairs_base <- split_char_by2(string_start)
  for (p in pairs_base)
    pair_counts[[p]] <- if_null(pair_counts[[p]], 0) + 1
  for (char in unique(transitions[, 2]))
    element_counts[[char]] <- 0
  for (char in strsplit(string_start, "")[[1]])
    element_counts[[char]] <- if_null(element_counts[[char]], 0) + 1

  for (i in seq_len(iters)) {
    correction <- hash::hash()
    for (rule in names(rules1)) {
      pair_counts_rule <- pair_counts[[rule]]
      if (isTRUE(pair_counts_rule > 0)) {
        rule_mapped_main <- rules1[rule]
        rule_mapped_left <- rules2[rule]
        rule_mapped_right <- rules3[rule]

        # remove original
        correction[[rule]] <- if_null(correction[[rule]], 0) - pair_counts_rule
        # left 2 cars
        correction[[rule_mapped_left]] <- if_null(correction[[rule_mapped_left]], 0) + pair_counts_rule
        # right 2 chars
        correction[[rule_mapped_right]] <- if_null(correction[[rule_mapped_right]], 0) + pair_counts_rule
        # new char between
        element_counts[[rule_mapped_main]] <- element_counts[[rule_mapped_main]] + pair_counts_rule
      }
    }
    for (corr in hash::keys(correction)) {
      pair_counts[[corr]] <- if_null(pair_counts[[corr]], 0) + if_null(correction[[corr]], 0)
    }
  }
  element_counts
}

#1
res1 <- impute_polymer_improved(d14_start, d14_trans, iters = 10L)
diff(range(hash::values(res1)))

#2
res2 <- impute_polymer_improved(d14_start, d14_trans, iters = 40L)
options(scipen = 99999)
diff(range(hash::values(res2)))
