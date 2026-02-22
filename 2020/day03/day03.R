library(tidyverse)

map <- readLines('2020/day03/input.txt') |>
  str_split_fixed('', n=Inf)

pos <- cbind(1:nrow(map),
             (seq(1, by=3, length.out=nrow(map))-1) %% ncol(map) + 1)

sum(map[pos] == "#")

# part 2:

slopes <- tribble(~x, ~y,
                  1, 1,
                  3, 1,
                  5, 1,
                  7, 1,
                  1, 2) |>
  mutate(rows = map(y, \(y) seq(1, by=y, to=nrow(map))),
         cols = map2(x, rows, \(x, y) seq(1, by=x, length.out=length(y)))) |>
  mutate(cols = map(cols, \(x) (x-1) %% ncol(map) + 1)) |>
  mutate(index = map2(rows, cols, \(x, y) data.frame(row=x, col=y))) |>
  pull(index) |> map(as.matrix)

map_int(slopes, \(x) sum(map[x] == "#")) |> prod()
