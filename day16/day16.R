library(tidyverse)
library(igraph)

# day 16 is another graph puzzle. Place the nodes on the edges like earlier.

# link up each edge on a square with a 1000 fold link in diamond pattern (if valid)
# link up across with 1 fold link.

# construct node list and edge list.
# nodes are edges in the map that join dot to dot
# edges then join nodes horizontally

input <- read.table(text="###############
#.......#....E#
#.#.###.#.###.#
#.....#.#...#.#
#.###.#####.#.#
#.#.#.......#.#
#.#.#####.###.#
#...........#.#
###.#.#####.#.#
#...#.....#.#.#
#.#.#.###.#.#.#
#.....#...#.#.#
#.###.#.#.#.#.#
#S..#.....#...#
###############", sep="", comment.char="") |> pull('V1')

input <- read.table(text="#################
#...#...#...#..E#
#.#.#.#.#.#.#.#.#
#.#.#.#...#...#.#
#.#.#.#.###.#.#.#
#...#.#.#.....#.#
#.#.#.#.#.#####.#
#.#...#.#.#.....#
#.#.#####.#.###.#
#.#.#.......#...#
#.#.###.#####.###
#.#.#...#.....#.#
#.#.#.#####.###.#
#.#.#.........#.#
#.#.#.#########.#
#S#.............#
#################", sep="", comment.char="") |> pull('V1')

input <- read_lines("day16/input.txt")

d <- input |> str_split_fixed("", n=Inf)
d_long <- d |> as.data.frame() |> tibble::rowid_to_column() |>
  pivot_longer(-rowid, names_to='colid') |>
  extract(colid, into='colid', regex="([[:digit:]]+)", convert=TRUE) |>
  rowid_to_column('vertex')

# each grid square becomes a diamond in our graph.
diamond <- tibble(subvertex=1:4, rowoff=c(0,0.5,0,-0.5), coloff=c(0.5,0,-0.5,0))
vertices <- d_long |> filter(value != "#") |> cross_join(diamond) |>
  tibble::rowid_to_column('id') |> mutate(locx = rowid + rowoff, locy = colid + coloff)

# ok, hook up edges within diamonds
edges_d <- vertices |> left_join(vertices, by=c("vertex", "colid", "rowid"),
                      relationship='many-to-many') |>
  filter(subvertex.x < subvertex.y) |>
  mutate(weight = if_else((coloff.x - coloff.y)*(rowoff.x - rowoff.y) == 0, 0.001, 1000))

# ok, now hookup between diamonds
edges_b <- vertices |> left_join(vertices, by=c('locx', 'locy'), relationship='many-to-many') |>
  filter(id.x < id.y) |>
  mutate(weight = 1)

ggplot(vertices) +
  geom_point(aes(x=locy, y=-locx), col='grey80') +
  geom_segment(data=edges_d, aes(x=locy.x, xend=locy.y, y=-locx.x, yend=-locx.y, col=as_factor(weight))) +
  geom_point(data=edges_b, aes(x=locy, y=-locx), col='darkblue') +
  theme_void() +
  scale_colour_manual(values=c('grey80','red')) +
  guides(col='none')

# ok, now create our graph
edge_comb <- bind_rows(edges_d, edges_b) |> select(id.x, id.y, weight)

graph <- graph_from_edgelist(edge_comb |> select(id.x, id.y) |> as.matrix(),
                             directed=FALSE) |>
  set_edge_attr('weight', value = edge_comb |> pull(weight))

# find the vertices to start/end at
start = vertices |> filter(value == "S", subvertex == 1) |> pull(id)
end   = vertices |> filter(value == "E") |> pull(id)

# find shortest path end vertex. We need only graph the first
dist <- distances(graph, v=start, to=end)
floor(min(dist))

# ok, part 2 we need to find the shortest paths
end <- end[which.min(dist)]
paths <- all_shortest_paths(graph, from=start, to=end)

# what are their costs?
visits <- paths$vpath |> map(as.integer) |>
  unlist() |> unique()

wch <- vertices |> slice(visits) |> select(rowid, colid) |>
  unique()

nrow(wch) # done!

# visualise the path
edges_visited <- paths$epaths |> unlist()

ggplot(vertices) +
  geom_point(aes(x=locy, y=-locx), col='grey80') +
  geom_segment(data=edges_d, aes(x=locy.x, xend=locy.y, y=-locx.x, yend=-locx.y, col=as_factor(weight))) +
  geom_point(data=edges_b, aes(x=locy, y=-locx), col='darkblue') +
  geom_segment(data=edges_d |> slice(edges_visited), aes(x=locy.x, xend=locy.y, y=-locx.x, yend=-locx.y), col='gold', size=2) +
  theme_void() +
  scale_colour_manual(values=c('grey80','red')) +
  guides(col='none')
