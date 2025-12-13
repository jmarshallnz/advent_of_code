library(tidyverse)
library(igraph)
options(digits = 22,
        pillar.max_dec_width = 22)

map <-
  "
7-F7-
.FJ|7
SJLL7
|F--J
LJ.LJ" |> str_split("\n") |> unlist() |>
  str_split_fixed('', n=Inf) |>
  as.data.frame()

map <- readLines("2023/day10/input.txt") |>
  str_split_fixed('', n=Inf) |>
  as.data.frame()

# convert our map to a graph based on the
# connectivitity structure

moves <- list(`-` = list(c(0,-1), c(0, 1)),
     `|` = list(c(-1,0), c(1, 0)),
     F = list(c(0,1),c(1,0)),
     `7` = list(c(0,-1), c(1,0)),
     L = list(c(0,1), c(-1,0)),
     J = list(c(0,-1), c(-1,0)))

get_edges <- function(dirs, letter) {
  from_nodes <- which(map == letter, arr.ind = TRUE)
  to_nodes1 <- cbind(from_nodes, sweep(from_nodes, 2, dirs[[1]], FUN='+'))
  map_dfr(dirs, \(x) cbind(from_nodes, sweep(from_nodes, 2, x, FUN='+')) |>
            as.data.frame())
}

edges <- imap_dfr(moves, get_edges)
names(edges) <- c('fx', 'fy', 'tx', 'ty')

map_dim <- dim(map)[1]

g <- edges |>
  filter(tx >= 1, tx <= map_dim, ty >= 1, ty <= map_dim) |>
  mutate(vt = (ty-1)*map_dim+tx,
         vf = (fy-1)*map_dim+fx) |>
  select(vf, vt) |>
  as.matrix() |>
  graph_from_edgelist()

plot(g, layout = layout_on_grid(g, width=nrow(dim), height=ncol(dim)))

start_node <- which(map == "S", arr.ind = TRUE) |>
  as.data.frame() |>
  set_names(c('fx', 'fy')) |>
  mutate(vf = (fy-1)*map_dim+fx) |>
  pull(vf)

# find components with strong connectivity (as the off-circuit paths
# will be connected from the part but not from the circuit)
circuit <- which(components(g, mode='strong')$membership == which.max(components(g, mode='strong')$csize))

# find distances from S: tricky bit is we only want 'out' but S isn't
# setup to find out?
neighbours = neighborhood(g, order=1, nodes=start_node, mindist=1, mode='in') |>
  unlist()

dists <- distances(g, v = neighbours, to = V(g), mode='out') |>
  apply(2, min) + 1

max(dists[is.finite(dists)])

# part 2 is a grid problem not a graph problem.

# we could use our circuit above as a starting point,
# but we need to know how we're running around it.

# this could plausibly be done by using the circuit and then
# moving around it to get the positions one by one, and then
# based on the position changes we know which direction we're
# going and can then figure out which "side" of the circuit
# we're on.

# BUT, TBH I think it's probably easier just to redo things
# using the grid directly.

# The key idea though is that we need only maintain which side
# is the "left" (or right) side of the circuit based on the direction
# we're going.

# it's easier therefore tracking both direction we're headed and
# our current position.

# first find the starting position and it's neighbours
s_pos <- which(map == "S", arr.ind = TRUE) |>
  as.numeric()

# find the neighbours that we can go to
start_nb <- matrix(s_pos + c(-1,0,1,0,0,-1,0,1), ncol=2, byrow=TRUE)
map[start_nb]

# OK, we're not allowed a J or a '.' so:
start_nb <- start_nb[c(2,4),]

# right, run around from one neighbour to the S, counting our length
moves <- tribble(~letter, ~direction, ~nd,
                 'L', 'D', 'R',
                 'L', 'L', 'U',
                 'F', 'U', 'R',
                 'F', 'L', 'D',
                 '7', 'R', 'D',
                 '7', 'U', 'L',
                 'J', 'D', 'L',
                 'J', 'R', 'U',
                 '-', 'R', 'R',
                 '-', 'L', 'L',
                 '|', 'U', 'U',
                 '|', 'D', 'D')

moves <- moves |> pivot_wider(names_from='direction', values_from='nd') |>
  tibble::column_to_rownames('letter') |>
  as.matrix()

dpos <- list('D' = c(1, 0), 'U' = c(-1, 0), 'L' = c(0, -1), 'R' = c(0, 1))

pos <- start_nb[1,, drop=FALSE]
dir <- "D"
loop <- list(list(pos = pos, dir = dir))
while (any(pos != s_pos)) {
  # do the move
  dir <- moves[map[pos], dir]
  pos <- pos + dpos[[dir]]
  # store current
  loop[[length(loop)+1]] <- list(pos = pos, dir = dir)
}

positions <- map(loop, 'pos') |> list_simplify()
directions <- map(loop, 'dir') |> list_simplify()

# part 1 check
nrow(positions) / 2

# OK, part 2 we need to append this with an inside/outside thing
# we can do this I think by adding to a separate list which items
# are on the right and which are on the left
left <- tribble(~letter, ~direction, ~l1, ~l2,
                 'L', 'D', '', '',
                 'L', 'L', 'D', 'R',
                 'F', 'U', 'L', 'U',
                 'F', 'L', '', '',
                 '7', 'R', 'U','R',
                 '7', 'U', '','',
                 'J', 'D', 'R','D',
                 'J', 'R', '','',
                 '-', 'R', 'U','',
                 '-', 'L', 'D','',
                 '|', 'U', 'L','',
                 '|', 'D', 'R','')

l1 <- left |> select(-l2) |> pivot_wider(names_from=direction, values_from=l1) |>
  tibble::column_to_rownames('letter') |>
  as.matrix()

l2 <- left |> select(-l1) |> pivot_wider(names_from=direction, values_from=l2) |>
  tibble::column_to_rownames('letter') |>
  as.matrix()

# this is the bit that figures out our (potential!) left positions.
get_left <- function(pos, dir) {
  at <- map[pos]
  if (at == 'S')
    return(NULL)
  lf <- rbind(dirs[[l1[at, dir]]],
              dirs[[l2[at,dir]]])
  if (is.null(lf))
    return(NULL)
  matrix(1, nrow=nrow(lf)) %*% pos + lf
}

pos <- start_nb[1,, drop=FALSE]
dir <- "D"
left <- get_left(pos, dir)
loop <- list(list(pos = pos, dir = dir, left = left))
while (any(pos != s_pos)) {
  # do the move
  dir <- moves[map[pos], dir]
  pos <- pos + dpos[[dir]]
  left <- get_left(pos, dir)
  # store current
  loop[[length(loop)+1]] <- list(pos = pos, dir = dir, left=left)
}

# ok, now fill in a copy of our map
positions <- map(loop, 'pos') |> list_simplify()

inside <- matrix(NA, nrow(map), ncol(map))
inside[positions] <- 2 # loop
image(inside)

left <- map(loop, 'left') |>
  map_dfr(data.frame) |> anti_join(data.frame(positions)) |>
  unique()

inside[as.matrix(left)] <- 1 # left
image(inside)

# ok, now flood fill from our 'left' via neighbours
nb <- tribble(~dx, ~dy,
             -1, 0,
             1, 0,
             0, -1,
             0, 1)

while(TRUE) {
  left_nb <- left |> cross_join(nb) |>
    mutate(X1 = X1 + dx,
           X2 = X2 + dy, .keep ='unused') |>
    anti_join(left) |>
    anti_join(data.frame(positions)) |>
    unique()
  cat("another ", nrow(left_nb), "neighbours\n")
  if (nrow(left_nb) == 0)
    break
  left <- bind_rows(left, left_nb)
}

# check:
inside[as.matrix(left)] <- 1 # left
image(inside)

# answer:
nrow(left)
sum(inside == 1, na.rm=TRUE)
