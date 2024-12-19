library(tidyverse)
library(igraph)

input <- read.table(text="5,4
4,2
4,5
3,0
2,1
6,3
2,4
1,5
0,6
3,3
2,6
5,1
1,2
5,5
2,5
6,5
1,4
0,4
6,4
1,1
6,1
1,0
0,5
1,6
2,0", sep=',')

input <- read.table("day18/input.txt", sep=",") + 1

# this is basically just a path finding problem again, so hookup our grid and
# go for it. Part 2 might be interesting then.

size <- 70+1
amount <- 1024
grid <- matrix(0, nrow=size, ncol=size)
grid[input |> slice(1:amount) |> as.matrix()] <- 1
# to a graph
grid_to_graph <- function(grid) {
  # any zeroes are hooked up
  ok <- which(grid == 0, arr.ind=TRUE) |> as_tibble() |>
    rowid_to_column('vertex')
  ok |> cross_join(ok) |>
    filter((row.x == row.y+1 & col.x == col.y) |
           (col.x == col.y+1 & row.x == row.y)) |>
    select(vertex.x, vertex.y) |>
    as.matrix() |>
    graph_from_edgelist(directed=FALSE) |>
    set_vertex_attr('x', value=ok |> pull(row)) |>
    set_vertex_attr('y', value=ok |> pull(col))
}

graph <- grid_to_graph(grid)

start <- which(vertex_attr(graph, 'x') == 1 & vertex_attr(graph, 'y') == 1)
end <- which(vertex_attr(graph, 'x') == size & vertex_attr(graph, 'y') == size)

distances(graph, v=start, to=end)

# Part 2: Fun! We don't want to iterate on this I think, but it
# possibly won't be too bad to do so if we can update our graph
# in reasonable time?

# Basically we start with the fully connected grid graph and
# the remove edges at each iteration.

# if we label the vertices with x and y attributes,
# then removing x,y just removes that vertex from the graph?

# then we can just check for "is connected". If we maintain
# components of the graph we can probably do things even
# better, but it's probably fast enough not to bother?

# first observation is input is all unique.
input |> unique() |> nrow() == input |> nrow()

# firstly, do we have articulation points in the graph? Yes, but ages away
cut_vertices <- graph |> articulation_points()

cut_points <- tibble(wch=cut_vertices |> as.numeric(), x=vertex_attr(graph, 'x', cut_vertices),
                     y=vertex_attr(graph, 'y', cut_vertices))

# ok, now check our input
to_go <- input[1025:nrow(input),] |> set_names(nm=c('x', 'y'))
cut_vertices <- to_go |> rowid_to_column('id') |>
  semi_join(cut_points) |>
  left_join(cut_points)

# These are very far away, so probably not a great set to go with.
# And they possibly don't even cut the graph where we need to anyway.

# We could find all the paths but my guess is there's a LOT of them,
# so that isn't efficient either.

# There's not a lot left though, so presumably we can just
# iterate through removing vertices.

connected_start_to_end <- function(graph) {
  start <- which(vertex_attr(graph, 'x') == 1 & vertex_attr(graph, 'y') == 1)
  end <- which(vertex_attr(graph, 'x') == size & vertex_attr(graph, 'y') == size)
  !is.infinite(distances(graph, v=start, to=end) |> as.numeric())
}

delete_vertex <- function(graph, v) {
  v <- which(vertex_attr(graph, 'x') == v$x & vertex_attr(graph, 'y') == v$y)
  delete_vertices(graph, v)
}

next_graph <- graph
for (i in 1:nrow(to_go)) {
  cat("deleting vertex ", i, "\n")
  next_graph <- next_graph |> delete_vertex(to_go[i,])
  if (!connected_start_to_end(next_graph))
    break
}
ans <- to_go |> slice(i)
ans |> mutate(done = paste(x-1,y-1,sep=","))

