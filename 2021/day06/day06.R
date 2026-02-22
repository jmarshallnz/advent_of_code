library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

fish <- readLines("2021/day06/input.txt") |>
  str_split(",") |> unlist() |> as.numeric()

#fish <- '3,4,3,1,2' |>
#  str_split(",") |> unlist() |> as.numeric()

# ok, we need to store our fish counts
counts <- map_int(0:8, \(x) sum(fish == x))

# each day we reduce the counts by 1 by rotating the counts to the left.
# 0 counts rotate into 8 counts as expected then?
for (i in 1:80) {
  # each day 1:8 gets mapped to 0..7, any 0's get mapped to 8 AND 6.
  counts <- counts[c(2:9, 1)] + c(rep(0,6), counts[1], rep(0, 2))
  cat('counts = ', counts, '\n')
}
sum(counts)

# for part 2 run for a further 256-80 days
for (i in 81:256) {
  # each day 1:8 gets mapped to 0..7, any 0's get mapped to 8 AND 6.
  counts <- counts[c(2:9, 1)] + c(rep(0,6), counts[1], rep(0, 2))
}
sum(counts)
