library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines('2021/day07/input.txt') |>
  str_split(',') |> unlist() |> as.integer()

# ok, now we want to find a number (presumably in the range)
# such that the distance from each number to that value is minimised

seq(min(input), max(input)) |>
  map_int(\(x) sum(abs(input-x))) |>
  min()

# part 2: cost is now the triangle function
tri_cost <- function(d) {
  d*(d+1)/2
}

seq(min(input), max(input)) |>
  map_int(\(x) sum(tri_cost(abs(input-x)))) |>
  min()
