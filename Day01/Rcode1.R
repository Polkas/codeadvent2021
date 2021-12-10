d1 <- readLines("Day01/input1")
#1
sum(diff(as.numeric(d1)) > 0)
#2
sum(diff(zoo::rollsum(as.numeric(d1), 3)) > 0)
