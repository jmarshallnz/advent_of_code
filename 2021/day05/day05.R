library(tidyverse)

input <- readLines('2021/day05/input.txt')

lines <- input |> str_split_fixed(pattern="[, >-]+", n=4) |>
  as.data.frame() |>
  mutate(across(everything(), as.integer)) |>
  setNames(c('x1', 'y1', 'x2', 'y2'))

# part 1
lines |>
  filter(x1 == x2 | y1 == y2) |>
  rowwise() |>
  mutate(lines = list(data.frame(x = x1:x2, y = y1:y2))) |>
  ungroup() |>
  unnest(lines) |>
  count(x, y) |>
  filter(n > 1) |>
  summarise(n = n())

# part 2: this is the same just without the filter?
lines |>
  rowwise() |>
  mutate(lines = list(data.frame(x = x1:x2, y = y1:y2))) |>
  ungroup() |>
  unnest(lines) |>
  count(x, y) |>
  filter(n > 1) |>
  summarise(n = n())
