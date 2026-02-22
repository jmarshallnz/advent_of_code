library(tidyverse)
library(igraph)

# classic shortest path trick
map <- readLines('2021/day15/input.txt') |>
  str_split_fixed('', n=Inf) |>
  apply(2, as.integer)

map <- '1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581' |> str_split('\n') |> unlist() |>
  str_split_fixed('', n=Inf) |>
  apply(2, as.integer)

# setup our igraph
vertices <- map |> as.data.frame() |> rowid_to_column('row') |>
  pivot_longer(-row) |>
  extract(name, into='col', regex="([0-9]+)", convert=TRUE) |>
  rowid_to_column('v')

shifted_down <- vertices |> mutate(row = row+1)
shifted_up <- vertices |> mutate(row = row-1)
shifted_left <- vertices |> mutate(col = col+1)
shifted_right <- vertices |> mutate(col = col-1)

edges <- vertices |>
  inner_join(bind_rows(shifted_down, shifted_up, shifted_left, shifted_right), join_by(row, col))

graph <- edges |>
  select(v.x, v.y) |>
  as.matrix() |>
  graph_from_edgelist(directed=TRUE) |>
  set_edge_attr('weight', value=edges |> pull(value.y))

start <- vertices |> filter(row == 1, col == 1) |> pull(v)
end   <- vertices |> filter(row == nrow(map), col == ncol(map)) |> pull(v)

distances(graph, v=start, to=end, mode='out')
p <- shortest_paths(graph, from=start, to=end, output = 'both',
                    mode='out')

edge_attr(graph, 'weight', index=p$epath[[1]]) |>
  sum()

# Hmm, that seems OK to me. The path length is about right?
# I guess we can try and plot it?
plot_graph <- graph |>
  set_edge_attr(name='size', value=1) |>
  set_edge_attr(name='size', index=p$epath[[1]], value=3) |>
  set_edge_attr(name='color', value='black') |>
  set_edge_attr(name='color', index=p$epath[[1]], value='red')

plot(plot_graph, layout=layout_on_grid(graph, width=100, height=100), vertex.size=1, vertex.label=NA,
       edge.width=edge_attr(plot_graph, 'size'),
     edge.color=edge_attr(plot_graph, 'color'),
     edge.arrow.size=0)

# Part 2: exactly the same on a 25 times larger graph..
bigmap <- matrix(0, nrow=nrow(map)*5, ncol=ncol(map)*5)
for (i in 0:4) {
  for (j in 0:4) {
    bigmap[i*100+1:100,j*100+1:100] <- (map+i+j-1) %% 9 +1
  }
}

vertices <- bigmap |> as.data.frame() |> rowid_to_column('row') |>
  pivot_longer(-row) |>
  extract(name, into='col', regex="([0-9]+)", convert=TRUE) |>
  rowid_to_column('v')

shifted_down <- vertices |> mutate(row = row+1)
shifted_up <- vertices |> mutate(row = row-1)
shifted_left <- vertices |> mutate(col = col+1)
shifted_right <- vertices |> mutate(col = col-1)

edges <- vertices |>
  inner_join(bind_rows(shifted_down, shifted_up, shifted_left, shifted_right), join_by(row, col))

graph <- edges |>
  select(v.x, v.y) |>
  as.matrix() |>
  graph_from_edgelist(directed=TRUE) |>
  set_edge_attr('weight', value=edges |> pull(value.y))

start <- vertices |> filter(row == 1, col == 1) |> pull(v)
end   <- vertices |> filter(row == nrow(bigmap), col == ncol(bigmap)) |> pull(v)

distances(graph, v=start, to=end, mode='out')

