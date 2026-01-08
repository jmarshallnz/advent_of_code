library(tidyverse)

input <- read.csv("2022/day04/input.txt", header=FALSE) |>
  extract(V1, into=c('s1', 'e1'), regex="([0-9]+)-([0-9]+)", convert = TRUE) |>
  extract(V2, into=c('s2', 'e2'), regex="([0-9]+)-([0-9]+)", convert = TRUE)

# part 1: intersect is one of the sets
input |>
  filter((s1 >= s2 & e1 <= e2) |
         (s2 >= s1 & e2 <= e1)) |>
  nrow()

# part 2: intersect is non-empty. OR: everything except those that intersect
input |>
  filter(!((s1 > e2) |
         (s2 > e1))) |>
  nrow()
