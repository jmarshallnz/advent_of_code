library(tidyverse)

buff <- readLines("2022/day06/input.txt") |>
  str_split("") |>
  unlist()

# part 1
d <- matrix(c(buff[-c(1:3)],
       buff[-c(1:2, length(buff))],
       buff[-c(1, length(buff)+1-1:2)],
       buff[-c(length(buff)+1-1:3)]), ncol=4) |>
  apply(1, \(x) length(unique(x)))

first(which(d == 4)) + 3

# part2: same thing but with 14 instead of 4
d <- matrix('', ncol=14, nrow=length(buff)-13)
for (i in 0:13) {
  d[,i+1] <- buff[-c(seq_len(i), length(buff)-seq_len(13-i))]
}
unique_d <- apply(d, 1, \(x) length(unique(x)))
first(which(unique_d == 14)) + 13
