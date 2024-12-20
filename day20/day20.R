library(tidyverse)
library(igraph)

# Day 20: A cheat is just removing a piece of wall.
# which is adding a vertex on the graph at (i,j) and
# linking to any of the vertices at (i-1,j), (i+1,j)
# (i,j-1) or (i,j+1).
# we can ignore walls on the outside.
input <- read.table(text="###############
#...#...#.....#
#.#.#.#.#.###.#
#S#...#.#.#...#
#######.#.#.###
#######.#.#...#
#######.#.###.#
###..E#...#...#
###.#######.###
#...###...#...#
#.#####.#.###.#
#.#...#.#.#...#
#.#.#.#.#.#.###
#...#...#...###
###############", sep="", comment.char="") |> pull(V1)

input <- read_lines("day20/input.txt")
d <- input |> str_split_fixed("", n=Inf) |> as_tibble()

# construct edge list: two points are joined if and only if they increase by one
d_long <- d[2:(nrow(d)-1), 2:(ncol(d)-1)] |> as.data.frame() |> tibble::rowid_to_column() |>
  pivot_longer(-rowid, names_to='colid') |>
  extract(colid, into='colid', regex="([[:digit:]]+)", convert=TRUE) |>
  rowid_to_column('vertex')

# ok, now join up all possible edges
horiz_edges <- d_long |> left_join(d_long |> mutate(colid = colid+1),
                                   by=c('colid', 'rowid'))
vert_edges <- d_long |> left_join(d_long |> mutate(rowid = rowid+1),
                                   by=c('colid', 'rowid'))

edges <- bind_rows(horiz_edges, vert_edges) |>
#  filter(!(value.x == '#' & value.y == '#')) |>
  mutate(weight = if_else(value.x != '#' & value.y != '#', 1, 10000)) |>
  select(x=vertex.x, y=vertex.y, weight) |>
  na.omit()

# Hmm, could we just have infinite (or very costly) weight of going through
# walls? That way we're just dropping the weights to 1 there.
graph <- graph_from_edgelist(edges |> select(x, y) |> as.matrix(), directed=FALSE) |>
  set_edge_attr('weight', value=edges |> pull(weight))

if (0) {
  library(ggraph)
  ggraph(graph, layout=layout_on_grid(graph)) +
    geom_edge_link(aes(edge_width=weight)) +
    geom_node_point()
}

start <- d_long |> filter(value == 'S') |> pull(vertex)
end   <- d_long |> filter(value == 'E') |> pull(vertex)

distances(graph, v=start, to=end) # yup, 84

# now, we're going to be changing an edge weight on the path right?
v <- shortest_paths(graph, from=start, to=end)$vpath[[1]]
# find all the edges adjacent to the path with the required weight

# we could get this list smaller by noting we need 2 steps to
# be back on the path. The weights are already there, so
# I think we can find the 2-neighbourhoods of each vertex in
# the path and see where they hit the a future vertex?

# we need only consider the graph induced by the path with
# edges of weight 10000, right?

# then we need to test how many of the vertices are 2-connected
# using only edges of weight 10000.
# can we invert the weights?

# i.e. make our weight along the path now 100 and the shortcut

# find the 2-neighbours of our path
path <- tibble(order=seq_along(v), id=v |> as.integer())

neighbours <- neighborhood(graph, order=2, nodes=v, mindist=2)

# ok, we now just need to know the distance of each of these
# in the path to see how much we save
saved <- neighbours |> map(as.integer) |> enframe(name='order', value='id') |>
  unnest(id) |> left_join(path, by='id') |>
  filter(!is.na(order.y)) |>
  mutate(saving = order.y - order.x - 2) |>
  filter(saving > 0) |>
  group_by(saving) |>
  summarise(n=n())

saved |> filter(saving >= 100) |>
  summarise(sum(n))

# part 2 ups the ante to 20 timesteps. We need to alter the graph
# to include all the edges now (done above), but I think the code should still
# work pretty much as-is. We'll just need to know how long the cheats
# are, which I think can just be done by doing cheats of length k
# at a time.

calc_saving_from_time <- function(k, graph, v) {
  # our path
  path <- tibble(order=seq_along(v), id=v |> as.integer())

  # neighbours of distance k
  neighbours <- neighborhood(graph, order=k, nodes=v, mindist=k)

  # time saved
  neighbours |> map(as.integer) |> enframe(name='order', value='id') |>
    unnest(id) |> left_join(path, by='id') |>
    filter(!is.na(order.y)) |>
    mutate(saving = order.y - order.x - k) |>
    filter(saving >= 100)
}

map_dfr(2:20, \(k) calc_saving_from_time(k, graph, v)) |>
  nrow()

# an easier way to solve this one given the path length
# isn't that big:

# 1. Find path
# 2. Get (x,y) coords
# 3. Crossjoin path and filter for forward
# 4. Find manhattan distance between points
# 5. Filter

p <- path |> left_join(d_long, join_by(id == vertex)) |>
  select(order, rowid, colid)

p_all <- p |> cross_join(p) |>
  filter(order.y > order.x) # cheat advances

p_all |>
  mutate(d_cheat = abs(rowid.x - rowid.y) + abs(colid.x - colid.y)) |> # shortcut
  filter(d_cheat <= 20) |>
  mutate(d_norm = order.y - order.x) |>
  mutate(d_imp = d_norm - d_cheat) |>
  filter(d_imp >= 100)
