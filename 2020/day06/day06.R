library(tidyverse)

input <- readLines('2020/day06/input.txt')

parsed <- tibble(input = input, gap = cumsum(input == "")) |>
  filter(input != "") |>
  mutate(input = str_split(input, '')) |>
  group_by(gap) |>
  mutate(person = row_number()) |>
  unnest(input) |>
  ungroup()

parsed |>
  count(gap, input) |>
  count(gap) |> summarise(sum(n))

# part 2
parsed |>
  group_by(gap) |>
  mutate(num = max(person)) |>
  count(gap, num, input) |>
  filter(n == num) |>
  nrow()
