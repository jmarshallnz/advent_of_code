library(tidyverse)

priority <- function(x) {
  x <- map_int(x, \(y) charToRaw(y) |> as.integer())
  ifelse(x >= charToRaw('a'), x - as.integer(charToRaw('a')) + 1,
         x - as.integer(charToRaw('A')) + 27)
}

# part 1
readLines("2022/day03/input.txt") |>
  str_split("") |>
  map_chr(\(x) intersect(x[1:(length(x)/2)],
                     x[1:(length(x)/2)+length(x)/2])) |>
  priority() |>
  sum()

# part 2: same thing but between three groups
three_way <- function(x, y, z) {
  intersect(intersect(x, y), z)
}

tibble(rucksacks = readLines("2022/day03/input.txt")) |>
  mutate(group = (row_number()+2) %/% 3) |>
  mutate(rucksacks = str_split(rucksacks, '')) |>
  group_by(group) |>
  summarise(d = do.call(three_way, args=rucksacks)) |>
  summarise(priority = priority(d) |> sum())
