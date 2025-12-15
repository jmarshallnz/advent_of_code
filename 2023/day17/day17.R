library(tidyverse)
library(collections)

options(digits = 22,
        pillar.max_dec_width = 22)

map <-
  "2413432311323
3215453535623
3255245654254
3446585845452
4546657867536
1438598798454
4457876987766
3637877979653
4654967986887
4564679986453
1224686865563
2546548887735
4322674655533" |> str_split("\n") |> unlist() |>
  str_split_fixed('', n=Inf) |>
  apply(2, as.numeric)

map <- readLines("2023/day17/input.txt") |>
  str_split_fixed('', n=Inf) |>
  apply(2, as.numeric)

# basically do a modified dijkstra with restriction
# on horizontal/vertical movement?
# dijkstra will maintain our 'best' distance
# but we also need to store how we got to a node
# using that distance as that's how we know what
# our neighbours are.

# This is part 2: Part 1 is basically the same
# but we change the moves

# I guess the path from any vertex to the end is really the path from
# the vertex _PLUS direction counter_ to the end?
# so if we index the BFS on that, maybe that would do the trick??
# i.e. we need to hold the distance from start to the (vertex, dircount)
# pair. Where dircount = R, RR, RRR, U, UU, UUU etc (12 options).
# as that dictates where we can move next.
#
# so in our dijkstra we need to know the node and direction thing
# I think this is doable...

# Part 1:
part <- 2
if (part == 1) {
  dir_lr <- expand_grid(dir=c('L', 'R'), rep=1:3) |>
    mutate(next_dir = map2(dir, rep, \(x, y) { a = if (y < 3) paste0(x, y+1) else NULL; c(a, 'U1', 'D1') } )) |>
    mutate(dir = paste0(dir, rep), .keep='unused')

  dir_ud <- expand_grid(dir=c('U', 'D'), rep=1:3) |>
    mutate(next_dir = map2(dir, rep, \(x, y) { a = if (y < 3) paste0(x, y+1) else NULL; c(a, 'L1', 'R1') })) |>
    mutate(dir = paste0(dir, rep), .keep='unused')
} else {
  dir_lr <- expand_grid(dir=c('L', 'R'), rep=1:10) |>
    mutate(next_dir = map2(dir, rep, \(x, y) { a = if (y < 10) paste0(x, y+1) else NULL; if (y > 3) c(a, 'U1', 'D1') else a })) |>
    mutate(dir = paste0(dir, rep), .keep='unused')

  dir_ud <- expand_grid(dir=c('U', 'D'), rep=1:10) |>
    mutate(next_dir = map2(dir, rep, \(x, y) { a = if (y < 10) paste0(x, y+1) else NULL; if (y > 3) c(a, 'L1', 'R1') else a })) |>
    mutate(dir = paste0(dir, rep), .keep='unused')
}

dirs <- bind_rows(dir_lr, dir_ud) |>
  deframe()

# we'll need our distances to also track approach direction
dist <- array(Inf, dim=c(nrow(map), ncol(map), length(dirs)), dimnames=list(NULL, NULL, names(dirs)))
#path <- matrix('', nrow=nrow(map), ncol(map))

# initialise the PQ with our start. Start can either be that we
# approached from the left or from down - two options I think.
dist[1, 1, 'D1'] <- 0 # hmm, what is the approach dir for this one?

dx <- c('D' = 1, 'U' = -1, 'L' = 0, 'R' = 0)
dy <- c('D' = 0, 'U' = 0, 'L' = -1, 'R' = 1)

pq <- priority_queue()
pq$push(list(row=1, col=1, dir='D1', dist=0), priority=0)

# process our queue
iter <- 0
while (pq$size() > 0) {

  # pop top of queue
  top <- pq$pop() # what is the dist though?

  # if it's not the shortest distance already, throw it away
  if (top$dist > dist[top$row, top$col, top$dir])
    next

  # check our neighbours. This depends on our current direction
  newdirs <- dirs[[top$dir]]

  for (move in newdirs) {
    # new position
    iter <- iter+1
    if (iter %% 10000 == 0)
      cat("iter:", iter, "\n")
    dir <- substr(move, 1, 1)
    row <- top$row + dx[dir]
    col <- top$col + dy[dir]
    if (row < 1 || row > nrow(map) ||
        col < 1 || col > ncol(map))
      next # ignore

    curr_dist <- dist[row, col, move]
    weight = map[row, col]
    if (top$dist + weight < curr_dist) {
      # found a shorter distance, update it
      new_dist <- top$dist + weight
      dist[row, col, move] <- new_dist
#      path[row, col] <- paste0(top$path, dir)
      # add this to our queue
      pq$push(list(row = row, col = col, dir = move, dist = new_dist), priority=-new_dist)
    }
  }
}

# find the distance to the end
dist[nrow(map), ncol(map),] |> min()
