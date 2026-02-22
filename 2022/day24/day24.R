library(tidyverse)
library(igraph)

input <- "#.######
#>>.<^<#
#.<..<<#
#>v.><>#
#<^v^^>#
######.#" |>
  str_split("\n") |> unlist()

input <- readLines("2022/day24/input.txt")
map <- input |>
  str_split_fixed('', n=Inf)

# 27 by 122 is 25 by 120, so every 600 it repeats.
# thus we can use Dijstrka on a directed graph of size 25*120*600 nodes
# where movement is always directed along the 600 length until the turn around.
# the end node is specified directly by posx,posy (so across all 600 states)

# In fact, it's going to be a much smaller graph as we can only move to a ".'
# and there's not that many!
sum(map == ".") # 272 nodes in the first layer compared to potential 3k or so.

# Strategy is: derive the map in the 600 time points until a repeat.
# For each map in sequence, derive neighbours and thus the directed graph.
# Link up the end nodes (we don't care which sequence we are at when we
# finish) with 0 weight links.
# Throw at shortest_path()
dirs <- tribble(~dir, ~dx, ~dy,
                '<', 0, -1,
                '>', 0, 1,
                '^', -1, 0,
                'v', 1, 0)

time_loop <- 600
time_loop <- 12

curr_pos <- dirs |>
  mutate(pos = map(dir, \(x) which(map == x, arr.ind = TRUE) |> as.data.frame())) |>
  unnest(pos)

first_vertices <- which(map == '.', arr.ind = TRUE) |>
  as.data.frame() |> mutate(loop = 1) |> tibble::rowid_to_column('vid')

wrap <- function(x, minx, maxx) {
  if_else(x < minx, maxx, if_else(x > maxx, minx, x))
}

all_vertices <- list()
all_vertices[[1]] <- first_vertices
all_edges <- list()

curr_vertices <- first_vertices
for (i in 2:time_loop) {
  # loop our curr_pos to next_pos
  next_pos <- curr_pos |> mutate(row = row + dx, col = col + dy) |>
    mutate(row = wrap(row, 2, nrow(map)-1),
           col = wrap(col, 2, ncol(map)-1))

  # ok, now map our current vertices to new vertices
  next_vertices <- expand_grid(row = 2:(nrow(map)-1), col = 2:(ncol(map)-1)) |>
    bind_rows(data.frame(row = c(1, nrow(map)), col = c(2, ncol(map)-1))) |>
    mutate(loop = i) |>
    anti_join(next_pos) |>
    mutate(lrow = row-1, urow = row+1, lcol = col-1, ucol = col+1) |>
    tibble::rowid_to_column('vid') |> mutate(vid = vid + max(curr_vertices$vid))

  # link up our current vertices with the next ones
  new_edges <- curr_vertices |> left_join(next_vertices, join_by(between(col, lcol, ucol), between(row, lrow, urow))) |>
    filter(abs(row.y - row.x) + abs(col.y - col.x) <= 1) |>
    select(vid.x, vid.y)

  # add our current vertices and edges to the list
  all_vertices[[length(all_vertices)+1]] <- next_vertices
  all_edges[[length(all_edges)+1]] <- new_edges

  # rotate around
  curr_pos <- next_pos
  curr_vertices <- next_vertices |> select(vid, row, col, loop)
}
# finally, we need to join our time 600 ones with time 1
curr_vertices
next_vertices <- first_vertices |>
  mutate(lrow = row-1, urow = row+1, lcol = col-1, ucol = col+1)

# link up our current vertices with the next ones
new_edges <- curr_vertices |> left_join(next_vertices, join_by(between(col, lcol, ucol), between(row, lrow, urow))) |>
  filter(abs(row.y - row.x) + abs(col.y - col.x) <= 1) |>
  select(vid.x, vid.y)

all_edges[[length(all_edges) + 1]] <- new_edges

# combine all our vertices/edges
vertices <- all_vertices |> bind_rows()
vertices |> summarise(n(), n_distinct(vid)) # fine!

edges <- all_edges |> bind_rows() # still really big!

# generate the graph
graph <- edges |> as.matrix() |> graph_from_edgelist(directed=TRUE)
plot(graph) # nice!

# and answer the question
from_vid <- vertices |> filter(row == 1, col == 2, loop == 1) |> pull(vid)
to_vid <- vertices |> filter(row == nrow(map), col == ncol(map)-1) |> pull(vid)

d <- distances(graph, v=from_vid, to=to_vid, mode='out')
min(d)
# Part 2 is a fun one! We have to first go forward, then backwards then forwards again.
# Going forward is easy - we're already done.
# Going backward then relies on rebuilding the graph backwards?
# Or do we have to rebuild backwards? I don't think so? We can just walk backwards, right?
from_vid2 <- to_vid[which.min(d)]
to_vid2 <- vertices |> filter(row == 1, col == 2) |> pull(vid)
d2 <- distances(graph, v=from_vid2, to=to_vid2, mode='out') # this isn't right: we'd be going backwards
# in time. We still need to go out!

from_vid3 <- to_vid2[which.min(d2)] # then need 272
to_vid3 <- to_vid
d3 <- distances(graph, v=from_vid3, to=to_vid3, mode='out')
min(d) + min(d2) + min(d3) # hmm, this isn't it? Wonder why...
