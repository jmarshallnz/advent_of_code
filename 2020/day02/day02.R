library(tidyverse)

input <- read.table("2020/day02/input.txt", sep=' ', header=FALSE) |>
  extract(V1, into=c('min', 'max'), regex="([0-9]+)-([0-9]+)", convert=TRUE) |>
  mutate(contains = str_sub(V2, 1, 1), password = str_split(V3, ''))

# OK, now do our mapping
input |>
  mutate(num = map2_int(contains, password, \(x, y) sum(y == x))) |>
  filter(num >= min, num <= max) |> nrow()

# Part 2
input |>
  mutate(entry1 = map2_chr(min, password, \(x, y) y[x]),
         entry2 = map2_chr(max, password, \(x, y) y[x])) |>
  filter(entry1 == contains | entry2 == contains) |>
  filter(entry1 != entry2) |>
  nrow()
