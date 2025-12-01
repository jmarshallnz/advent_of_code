library(tidyverse)
library(igraph)

input <- read_lines("day10/input.txt")

d <- input |> str_split_fixed("", n=Inf) |> apply(1, as.numeric)

# construct edge list: two points are joined if and only if they increase by one
d_long <- d |> as.data.frame() |> tibble::rowid_to_column() |>
  pivot_longer(-rowid, names_to='colid') |>
  extract(colid, into='colid', regex="([[:digit:]]+)", convert=TRUE) |>
  rowid_to_column('vertex')

edges <- d_long |> cross_join(d_long) |>
  filter((abs(rowid.x - rowid.y) == 1 & colid.x == colid.y) |
         (abs(colid.x - colid.y) == 1 & rowid.x == rowid.y)) |>
  filter(value.y == value.x + 1) |>
  select(vertex.x, vertex.y) |> as.matrix()

vertices <- d_long |> select(vertex, value)

graph <- graph_from_edgelist(edges, directed=TRUE)

origins <- d_long |> filter(value == 0) |>
  pull(vertex)

dest <- d_long |> filter(value == 9) |>
  pull(vertex)

dist <- distances(graph, v=origins, to=dest, mode='out')
sum(dist == 9)

# part 2 needs the number of paths
num_paths <- function(graph, from, dest) {
  all_shortest_paths(graph, from=from, to=dest, mode='out') |>
    pluck('vpaths') |> length()
}

map_int(origins, ~num_paths(graph, from=., dest)) |>
  sum()
