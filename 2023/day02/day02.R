library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines('2023/day02/input.txt') |>
  str_split_fixed(":", n=2) |>
  as.data.frame() |>
  set_names(c("game", "input"))

games <- input |> extract('game', into='game', regex = "([[:digit:]]+)", convert=TRUE) |>
  mutate(in_split = str_split(input, ";")) |>
  mutate(cubes = map(in_split, \(x) str_split(x, pattern=","))) |>
  unnest(cubes) |>
  tibble::rowid_to_column('selection') |>
  unnest(cubes) |>
  extract(cubes, into=c('number', 'color'), "([[:digit:]]+) (red|green|blue)", convert=TRUE) |>
  select(game, selection, number, color) |>
  pivot_wider(names_from=color, values_from=number, values_fill = 0)

games |>
  group_by(game) |>
  summarise(across(red:blue, max)) |>
  filter(red <= 12, green <= 13, blue <= 14) |>
  summarise(sum(game))

games |>
  group_by(game) |>
  summarise(across(red:blue, max),
            power = red*green*blue) |>
  summarise(sum(power))
