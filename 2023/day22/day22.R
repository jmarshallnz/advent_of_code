library(tidyverse)

input <- "1,0,1~1,2,1
0,0,2~2,0,2
0,2,3~2,2,3
0,0,4~0,2,4
2,0,5~2,2,5
0,1,6~2,1,6
1,1,8~1,1,9" |>
  str_split("\n") |> unlist() |>
  str_split_fixed("[,~]", n=6) |>
  apply(2, as.numeric)

input <- readLines("2023/day22/input.txt") |>
  str_split_fixed("[,~]", n=6) |>
  apply(2, as.numeric)

# ok, figure out which bricks are above other bricks in space
# our input isn't large, so we could do this by expanding
# into multiple 3D blocks and then testing all pairs for
# adjacency in Z (which happens when x and y are the same and Z is one off)

bricks <- input |>
  as.data.frame() |>
  set_names(nm=c('x1', 'y1', 'z1', 'x2', 'y2', 'z2')) |>
  arrange(pmin(z1, z2)) |>
  rowwise() |>
  group_split()

# iterate through and drop the blocks. We can drop them as far as the previous
# layered blocks
dropped_blocks <- list()
heights <- matrix(0, 10, 10)

# iterate through our list and drop the bricks down
for (brick in bricks) {
  block <- expand_grid(x=brick$x1:brick$x2,
              y=brick$y1:brick$y2,
              z=brick$z1:brick$z2)
  xy <- block |> select(x,y) |> as.matrix()
  minz <- max(heights[xy+1])
  block$z = block$z - min(block$z) + minz + 1
  heights[xy+1] <- max(block$z)
  dropped_blocks[[length(dropped_blocks)+1]] <- block
}

# ok, now find supporting blocks
dropped_blocks <- bind_rows(dropped_blocks, .id='brick')
dropped_blocks_above <- dropped_blocks |> mutate(z = z-1)

supporter <- dropped_blocks |> left_join(dropped_blocks_above, join_by(x==x, y==y, z == z)) |>
  filter(is.na(brick.y) | brick.x != brick.y)

# so 1 is suporting 2,3
# 2 is supporting 4,5
# 3 also supporting 4,5
# 4 supporting 6
# 5 supporting 6
# 7 supporting noone

ok_to_remove <- supporter |> add_count(brick.y) |>
  group_by(brick.x) |>
  add_count(brick.y) |>
  mutate(supported = n - nn > 0) |>
  group_by(brick.x) |>
  summarise(removable = all(supported)) # can remove them

ok_to_remove |>
  filter(removable) |>
  nrow()

# part 2:
#
# time for some graph theory!
#
# how connected is each brick to the others in a directed sense
#
# if we have distance between blocks on graph then that's prob enough?
# i.e. distance matrix from all nodes to all nodes?
# then just sum down it

adj_list <- dropped_blocks |> left_join(dropped_blocks_above, join_by(x==x, y==y, z == z)) |>
  filter(brick.x != brick.y) |>
  select(brick.x, brick.y) |>
  mutate(across(everything(), as.numeric)) |>
  nest(data=brick.y) |>
  deframe() |>
  map(\(x) x |> pull(brick.y))

disintegrates <- function(x) { # x is a list of vertices we've disintegrated
  nb <- adj_list[x] |> unlist() |> unique() # ones that possibly disintegrate next
  # remove any that are still supported by those other than x
  gone <- setdiff(nb, adj_list[-x] |> unlist() |> unique())
  if (length(gone) == 0) done(x) else c(x, gone)
}

remove_brick <- function(brick) {
  cat("removing brick", brick, "\n")
  setdiff(reduce(seq_along(adj_list), \(x, y) disintegrates(x), .init=brick), brick)
}

gone <- 1:10 |> map(remove_brick)
gone <- .Last.value
gone |> lengths() |> sum()

remove_vertex <- function(graph, v) {
  del <- V(graph)[id == v] |> as.integer() # find id to delete
  while (TRUE) {
    nb_ind <- graph |> neighborhood(order=1, nodes=del, mode='out', mindist=1) |> unlist()
    nb_nm <- vertex_attr(graph, 'id', nb_ind)
    g <- graph |> delete_vertices(del)
    nb_g_ind <- V(g)[id %in% nb_nm] |> as.integer()
    nb_g_nm <- vertex_attr(g, 'id', nb_g_ind)
    nb <- nb_g_nm[neighborhood_size(g, mode='in', nodes=nb_g_ind, mindist=1) == 0]
    if (length(nb) == 0)
      break
    del <- c(del, nb)
  }
  setdiff(del, v)
}

graph <- dropped_blocks |> left_join(dropped_blocks_above, join_by(x==x, y==y, z == z)) |>
  filter(brick.x != brick.y) |>
  select(brick.x, brick.y) |>
  mutate(across(everything(), as.numeric)) |>
  as.matrix() |>
  graph_from_edgelist()

graph <- graph |> set_vertex_attr('id', value=1:length(V(graph)))

gone <- map(1:length(V(graph)), \(v) remove_vertex(graph, v))
gone |> lengths() |>
  sum() #61555
