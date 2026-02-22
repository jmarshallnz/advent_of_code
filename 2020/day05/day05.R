library(tidyverse)

input <- readLines('2020/day05/input.txt') |>
  str_split_fixed('', n=Inf)

row <- apply(input[,1:7] == "B", 1, \(x) sum(2^((6:0))*x))
col <- apply(input[,8:10] == "R", 1, \(x) sum(2^((2:0))*x))
max(row*8+col)

#part 2
sorted <- sort((row*8)+col)

wch <- which(diff(sorted) == 2)
sorted[c(wch-1,wch,wch+1)]
