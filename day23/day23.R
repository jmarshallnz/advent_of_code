library(tidyverse)
library(igraph)

graph <- read.table("day23/input.txt", sep="-") |>
  as.matrix() |>
  graph_from_edgelist(directed=FALSE)

# part 1: find 3-cliques
graph |> cliques(min=3, max=3) |>
  unlist() |> names() |> enframe() |>
  unnest(value) |>
  filter(str_starts(value, 't')) |>
  summarise(n_distinct(name))

# part 2: largest clique
graph |> largest_cliques() |>
  unlist() |> names() |>
  sort() |> paste(collapse=',')
