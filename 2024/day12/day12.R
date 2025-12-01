library(tidyverse)
library(igraph)

input <- read.table(text=
"RRRRIICCFF
 RRRRIICCCF
 VVRRRCCFFF
 VVRCCCJFFF
 VVVVCJJCFE
 VVIVCCJJEE
 VVIIICJJEE
 MIIIIIJJEE
 MIIISIJEEE
 MMMISSJEEE", sep="") |>
  pull("V1")

input <- read_lines("day12/input.txt")

d <- input |> str_split_fixed("", n=Inf)

# construct edge list: two points are joined if and only if they share
# a common letter horizontally or diagonally
d_long <- d |> as.data.frame() |> tibble::rowid_to_column() |>
  pivot_longer(-rowid, names_to='colid') |>
  extract(colid, into='colid', regex="([[:digit:]]+)", convert=TRUE) |>
  rowid_to_column('vertex')

# Hmm, a cross join here is probably going to be painful. We need only
# match on common values though
edges <- d_long |> left_join(d_long, by='value', relationship='many-to-many') |>
  filter((abs(rowid.x - rowid.y) == 1 & colid.x == colid.y) |
         (abs(colid.x - colid.y) == 1 & rowid.x == rowid.y)) |>
  select(vertex.x, vertex.y)

vertices <- d_long |> select(vertex, value)

graph <- graph_from_edgelist(edges |>
                               filter(vertex.x > vertex.y) |> # don't need directed
                               as.matrix(), directed=FALSE)

#plot(graph, layout=layout_on_grid(graph))

# connected components
comp <- components(graph)

# perimeters: These are the number of edges *not* present in the graph.
# each vertex should have degree 4, but don't. The difference is the fence
tibble(fences = 4-degree(graph), region=comp$membership) |>
  group_by(region) |>
  summarise(area = n(), perimeter = sum(fences)) |>
  summarise(sum(area*perimeter))

# Part 2 the perimeter is calculated using sides. How can we efficiently
# do that? Well, sides need to be continuous I guess.

# Hmm, could we find the edges we will have between our components (i.e. fence bits) and
# then check which ones are linked (i.e. in the same horizontal/vertical, and one apart)

# we do this by finding all possible edges and then removing the internal ones via an anti_join
horiz_edges <- bind_rows(d_long |> mutate(rowid.y = rowid + 1),
                         d_long |> mutate(rowid.y = rowid - 1)) |>
  left_join(d_long, join_by(rowid.y == rowid, colid == colid))

vert_edges <- bind_rows(d_long |> mutate(colid.y = colid + 1),
                        d_long |> mutate(colid.y = colid - 1)) |>
  left_join(d_long, join_by(rowid == rowid, colid.y == colid))

horiz_fence <- horiz_edges |> anti_join(edges) |>
  left_join(tibble(vertex.x = V(graph) |> as.integer(), comp = comp$membership)) |>
  select(rowid.x = rowid, rowid.y, colid, comp)

vert_fence <- vert_edges |> anti_join(edges) |>
  left_join(tibble(vertex.x = V(graph) |> as.integer(), comp = comp$membership)) |>
  select(rowid, colid.x = colid, colid.y, comp)

# ok, now for each component, throw away. fence pieces if they're adjacent
# to another other (i.e. same row or column and, exactly 1 apart)
bind_rows(horiz_fence |> anti_join(horiz_fence |> mutate(colid = colid+1)),
          vert_fence |> anti_join(vert_fence |> mutate(rowid = rowid+1))) |>
  group_by(comp) |>
  summarise(perimeter = n()) |>
  left_join(enframe(comp$csize, name = 'comp')) |>
  summarise(sum(perimeter*value))
