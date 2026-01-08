library(tidyverse)
library(igraph)

map <- 'Sabqponm
abcryxxl
accszExk
acctuvwj
abdefghi' |> str_split("\n") |> unlist() |> str_split_fixed('', n=Inf)

map <- readLines("2022/day12/input.txt") |>
  str_split_fixed('', n=Inf)

# ok, now setup our adjacencies
vertices <- map |> as.data.frame() |> tibble::rowid_to_column('row') |>
  pivot_longer(-row, names_to='col', values_to='value') |>
  extract(col, into='col', regex="([0-9]+)", convert=TRUE) |>
  tibble::rowid_to_column('v') |>
  mutate(h = map_int(value, \(x) { x <- ifelse(x == 'S', 'a', x); x <- ifelse(x == 'E', 'z', x); as.integer(charToRaw(x)) }))

start <- vertices |> filter(value == 'S') |> pull(v)
end <- vertices |> filter(value == 'E') |> pull(v)
g <- vertices |> left_join(vertices |> mutate(row_l = row-1, row_r = row+1, col_l = col-1, col_r = col+1),
               join_by(between(row, row_l, row_r),
                       between(col, col_l, col_r))) |>
  filter((row.x - row.y)*(col.x - col.y) == 0,
         v.x != v.y,
         h.y <= h.x+1) |>
  select(v.x, v.y) |>
  as.matrix() |>
  graph_from_edgelist()

distances(g, v = start, to = end, mode='out')

# part 2: straight forward: find distance from any 'a' vertex to the end, and then find the shortest
starts <- vertices |> filter(value %in% c('S', 'a')) |> pull(v)

tibble(start = starts, dist = distances(g, v=starts, to=end, mode='out') |> as.numeric()) |>
  filter(is.finite(dist)) |>
  arrange(dist)
