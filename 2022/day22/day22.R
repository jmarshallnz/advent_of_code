library(tidyverse)

input <- "        ...#
        .#..
        #...
        ....
...#.......#
........#...
..#....#....
..........#.
        ...#....
        .....#..
        .#......
        ......#.

10R5L5R10L4R5L5" |> str_split("\n") |> unlist()

input <- readLines("2022/day22/input.txt")


map <- input[1:(which(input == "")-1)] |>
  str_split_fixed('', n=Inf)

steps <- input[length(input)] |> str_split("[LR]") |> unlist()
turns <- input[length(input)] |> str_split("[0-9]+") |> unlist()

facing <- 'U' # start facing up so our first turn to the right makes us face right

# our direction can be inferred from the current direction
dir_changes <- matrix(c('R', 'L',
                        'L', 'R',
                        'U', 'D',
                        'D', 'U'), nrow=4, byrow=TRUE, dimnames = list(facing = c('U', 'D', 'L', 'R'), turn=c('R', 'L')))

# ok, now accumlate up our facing direction
moves <- tibble(turn = c("R", turns[turns != ""]), steps = as.numeric(steps)) |>
  mutate(facing = accumulate(turn, \(x, y) dir_changes[x, y], .init = 'U')[-1])

# state is which node we're at and which way we're facing.
# at the start we need only traverse our graph as stated.
# part 2 might be some path weirdness maybe?

wrap_L <- apply(map, 1, \(x) first(which(x %in% c(".", '#'))))
wrap_R <- apply(map, 1, \(x) ncol(map)+1-first(which(rev(x) %in% c('.', '#'))))
wrap_U <- apply(map, 2, \(x) first(which(x %in% c(".", '#'))))
wrap_D <- apply(map, 2, \(x) nrow(map)+1-first(which(rev(x) %in% c('.', '#'))))

wrap_row <- function(pos, row) {
  if (pos < wrap_U[row])
    pos = wrap_D[row]
  if (pos > wrap_D[row])
    pos = wrap_U[row]
  pos
}

wrap_col <- function(pos, col) {
  if (pos < wrap_L[col])
    pos = wrap_R[col]
  if (pos > wrap_R[col])
    pos = wrap_L[col]
  pos
}

wrap_D
delta_row <- c(U = -1, D = 1, R = 0, L = 0)
delta_col <- c(U = 0, D = 0, R = 1, L = -1)
row <- 1
col <- first(which(map[1,] == '.'))
dir <- 'U'
# each move we turn then step in our current direction.
state <- list(c(row, col))
for (i in 1:nrow(moves)) {
  move = moves[i,]
  # turn
  dir = dir_changes[dir, move$turn]
  # now step
  for (j in 1:move$steps) {
    # try stepping with wrapping
    next_row = wrap_row(row + delta_row[[dir]], col)
    next_col = wrap_col(col + delta_col[[dir]], row)
    # check what tile we're on. If we are at a wall
    # we need to break where we are
    if (map[next_row, next_col] == "#") { # hit a wall
      break
    }
    row = next_row; col = next_col
    state <- c(state, list(c(row, col)))
  }
}

facing_to_num <- c('R' = 0, 'D' = 1, 'L' = 2, 'U' = 3)
1000 * row + 4 * col + facing_to_num[dir]

path <- list_transpose(state) |>
  as.data.frame() |> set_names(c('x', 'y'))

print_map <- map
print_map[path |> as.matrix()] <- "X"
print_map[path |> slice(nrow(path)) |> as.matrix()] <- "E"

# part 2 the wrapping rules are different. Fun! When we wrap we change location AND facing now

# Doing a lot of thinking, the likely best way to go here is two options:
# 1. Use the shape of the input to hardcode the way the wrapping operates. Noting
#.   that the shape differs for the example and final input. This is bad.
# 2. Do a "general" solution for the shape. This is hard, but potentially
#.   the 'easiest' option is mapping a cube to the 'cubenet'. The reason
#    this is a good idea is that movement on the cube is really easy: When
#    you're on a face, if you move to another face you're always walking AWAY
#.   from the previous face. i.e. change in direction is to the normal vector
#.   of the previous face. So direction is easy. What _isn't_ easy is setting
#.   up the initial map. But, I think this is not too bad, as again you can
#.   "roll" the cube around the cube net I think? e.g. if you're on a "tile"
#.   or face of the cubenet and you move LEFT, you're rolling the cube around
#    the X axis: the X coordinate direction doesn't change, but Y now becomes Z
#.   and our Normal is now Y (or -Y). We can do this with rotationg matrices.
#.   So we roll the cube around, and for each one where we hit a tile we get a new basis in 3D for our points.
#.   This basis is basically the orientation.
#.   We can map our original a, b + tile_offset coordinates of the tile to 3D coordinates a X + b Y + Z.
#.   Or possible -1*Z?

# as we're going around the cubenet, we hit an edge and transistion from Face X to Face Y (defined on the cube AND
# the cubenet). This transistions from X,Y,Z -> X',Y',Z' and thus from aX+bY+(-?)Z (for given a,b which would be
# scaled in-face tile coordinates) to a'X' + b'Y' + Z'. We can solve this just by stepping to cube space using
# the orientation matrix of the currnet face, then stepping back using the new face orientation.
# To find the new face we can use the current direction: we'd be moving to the face whose Z direction is
# our current direction projected into cube space.
# The NEW direction would be the Z direction of the previous face which is quite neat.

# So all we need to know is how our basis moves around as the cube rolls.
# And then we map our current position on the cube (i.e. in our cube basis) to the next face's basis.
# by inverting. This gives our coordinates a, b on the next face. We can also get the direction by
# noting that it'll be Z.
# Basis of first = [X, Y, Z]

# one tricky thing is the rolling around the map: we need to roll the cube in it's current basis.

# possibly the cross-product might work as we're rotating only by 90 degrees??

# find the dimension of the map
face_size <- sqrt(sum(map %in% c('.', '#'))/6)
coord_max <- face_size-1; coord_step = 2

# OK, now our coordinate system for our tiles will be -coord_max .. coord_max in steps of 2.
# centered at steps of face_size+1

tile_max_x <- nrow(map)/face_size
tile_max_y <- ncol(map)/face_size

# our first face tile is the one where we start, so find that
col <- first(which(map[1,] == '.'))%/%face_size
first_face <- list(offset = matrix(c(0, col), nrow=1), orientation = diag(1,3))

# OK, progressively roll out our cube (flood fill) rotating the basis as required

rotX = matrix(c(1,0,0,0,0,1,0,-1,0), nrow=3) # rotate around X 90 degrees
rotY = matrix(c(0,0,-1,0,1,0,1,0,0), nrow=3) # rotate around Y 90 degrees
rotZ = matrix(c(0,1,0,-1,0,0,0,0,1), nrow=3) # rotate around Z 90 degrees

rots <- list(rotX, rotY, rotZ)
rotate_orientation <- function(o, wch, dir) {
  # orientation o is rotated around it's second member
  wch_rot <- which(o[,wch] != 0)
  rot <- rots[[wch_rot]]
  dir <- dir * o[wch_rot, wch]
  if (dir < 0) rot <- t(rot)
  rot %*% o
}

library(collections)
# we index our faces by Z (the normal to the face)
faces <- dict(items = list(first_face), keys = list(first_face$orientation[,3,drop=FALSE])) # indexed by Z

# flood fill the cubenet via a process stack remembering if we've visited
to_process <- stack(list(first_face))
visited <- matrix(FALSE, nrow=tile_max_x, ncol=tile_max_y)
visited[first_face$offset+1] <- TRUE # visited this face
while(to_process$size()) {
  # pop currently face off our stack
  cur_face <- to_process$pop()
  # roll to it's neighbours by rotating through X or Y
  nbs = list(
         list(offset = cur_face$offset + c(1, 0),
              orientation = rotate_orientation(cur_face$orientation, 2, -1)), # down # I think this needs rotate about the CURRENT y, not rotate about the fixed Y
         list(offset = cur_face$offset + c(-1, 0),
              orientation = rotate_orientation(cur_face$orientation, 2, 1)), # up
         list(offset = cur_face$offset + c(0, -1),
              orientation = rotate_orientation(cur_face$orientation, 1, -1)), # left # first time we get there we want to rotate about _existing_ X (which will be Z)
         list(offset = cur_face$offset + c(0, 1),
              orientation = rotate_orientation(cur_face$orientation, 1, 1))) # right

  for (nb in nbs) {
    # 1. check if in the bounds of the map
    if (any(nb$offset < 0 | nb$offset >= c(tile_max_x, tile_max_y)))
      next
    # 2. check if not empty (top left char)
    if (!(map[nb$offset*face_size + c(1,1)] %in% c("#", ".")))
      next
    # 3. check if not already visited (by this time we've sense-checked range)
    if (!visited[nb$offset+1]) {
      # add to our face list and to_process list
      faces$set(nb$orientation[,3,drop=FALSE], nb) # add the face
      visited[nb$offset+1] <- TRUE
      to_process$push(nb)
    }
  }
}

# yay, have our 6 faces.

# now we can walk around the map.
# We do it by first walking around a face in the given direction. Once we hit
# out of bounds on an edge we use DIRECTION to infer which face we're going to
# and then map the current location on the cube to the new face (the point in
# cubespace stays the same, all that changes is the face coordinates).
face <- faces$get(first_face$orientation[,3,drop=FALSE])
pos <- c(1, 1)

#facing_to_num <- c('R' = 0, 'D' = 1, 'L' = 2, 'U' = 3)
dir <- c(-1,0) # up so we turn to right straight away
tile_pos <- function(face, pos) {
  face$offset*face_size+pos
}

# each move we turn then step in our current direction.

turn_mat <- list(L = rotZ[1:2,1:2], R = t(rotZ)[1:2,1:2])

state <- list(list(face=face, pos=pos))
for (i in 1:nrow(moves)) {
  move = moves[i,]
  # turn
  dir = as.numeric(turn_mat[[move$turn]] %*% dir)
  # now step
  for (j in 1:move$steps) {
    # try stepping with wrapping
    next_pos = pos + dir
    next_face = face
    next_dir = dir
    # if we're out of bounds
    if (any(next_pos < 1 | next_pos > face_size)) {
      # OOB, move to the next face with normal of the negated current direction (normals point in)
      z_to_find <- face$orientation %*% -c(dir, 0)
      next_face = faces$get(z_to_find)

      # ok, we have our next_face, compute our next coordinates
      scaled_coords <- c((pos-(face_size+1)/2)/(face_size-1)*2, -1) # hmm, it's not necessarily -1 though?
      cube_coord <- face$orientation %*% scaled_coords
      scaled_next_pos_inc_z <- t(next_face$orientation) %*% cube_coord
      next_pos <- round(scaled_next_pos_inc_z[-3]*(face_size-1)/2+(face_size+1)/2)
      # next direction is then our previous Z projected into this space?
      cube_prev_dir <- face$orientation %*% c(0, 0, 1)
      next_dir_inc_z <- t(next_face$orientation) %*% cube_prev_dir
      next_dir <- next_dir_inc_z[-3] # need to look that back up to go backwards?
    }
    # check what tile we're on. If we are at a wall
    # we need to break where we are
    if (map[tile_pos(next_face, next_pos)] == "#") { # hit a wall
      break
    }
    pos = next_pos
    dir = next_dir; face = next_face;
    state <- c(state, list(list(face=face, pos=pos)))
  }
}

# do the plotting
plot_map <- matrix(0, nrow=nrow(map), ncol=ncol(map))
plot_map[map %in% c("#", ".")] <- 1
for (s in seq_along(state)) {
  sh <- state[[s]]
  plot_map[tile_pos(sh$face, sh$pos)] <- 2+s
}
image(t(plot_map[nrow(plot_map):1,]))

# compute the answer
# We need to switch our dir back
dir_score <- -1
dir_score <- dict(items = as.list(0:3), keys = list(c(0,1),c(1, 0),c(0,-1),c(-1,0)))

sum(tile_pos(face, pos) * c(1000, 4)) + dir_score$get(dir)

