d7 <- readLines("Day7/input7")
d7_h <- as.numeric(strsplit(d7, ",")[[1]])

#1
sum(abs(d7_h - median(d7_h)))
#2
min(sapply(0:max(d7_h), function(x) sum(sapply(abs(d7_h - x), function(y) (y+1)/2 * y))))
