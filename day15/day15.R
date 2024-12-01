library(tidyverse)

in_map <- read.table(text="#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######", sep="", comment.char="") |> pull(V1)

in_moves <- "<vv<<^^<<^^"

in_map <- read.table(text="##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########", sep="", comment.char="") |> pull(V1)

in_moves <- "<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^"

input <- read_lines("day15/input.txt")
empty_line <- which(input == "")

in_map <- input[1:(empty_line-1)]
in_moves <- input[(empty_line+1):length(input)] |> paste0(collapse='')

directions <- in_moves |> str_remove_all("\n") |> str_split("") |> unlist()
map <- in_map |> str_split_fixed("", n=Inf)

deltas <- tibble(move = c("<", ">","^","v"), x=c(0,0,-1,1), y=c(-1,1,0,0))


moves <- directions |> as_tibble() |>
  left_join(deltas, by=c(join_by(value == move))) |>
  select(x,y) |> rowwise() |> group_split() |> map(as.matrix)

# the map really needs to have information on where the walls are,
# where the guard is (position) and where the rocks are.
# this could be sparse, but for now let's just do it with a matrix

do_move <- function(map, move) {
  curr_pos <- which(map == "@", arr.ind = TRUE)
  next_map <- map
  next_pos <- curr_pos + move
  moving <- '@' # moving the guard
  prev   <- '.'
  while (map[next_pos] == "O") { # a box, need to move it
    next_map[curr_pos] <- prev; next_map[next_pos] <- moving;
    curr_pos <- next_pos
    next_pos <- curr_pos + move
    prev   <- moving
    moving <- 'O' # moving boxes
  }
  if (map[next_pos] == ".") {
    # can complete our move
    next_map[curr_pos] <- prev; next_map[next_pos] <- moving;
    return(next_map)
  }
  # if we get here then we don't have an empty square, return original map
  return(map) # couldn't finish the move
}

for (i in seq_along(moves)) {
  map <- do_move(map, moves[[i]])
}

gps <- function(map) {
  sum((which(map == "O", arr.ind = TRUE) - 1) %*% c(100,1))
}

gps(map)

# in part two we have wider boxes. The same logic will work, except
# we may now potentially be pushing many more boxes. So the moves accumulate
# and we need to check each one. i.e. instead of just moving one 'character'
# we need to move a bunch of them at once, and all of them have to be OK
# for it to work.

# while it's probably doable with a character matrix, tracking which 'pixels'
# need to move and which ones don't (e.g. you can imagine a diagonal line of
# blocks being moved at once) is likely a bunch easier if we just maintain
# a list of blocks, and a position of the guard.

# so our 'map' will be a list['guard'] = position and list['blocks'] with a
# list (data.frame?) of blocks.

# our move list would then be a guard position and a list of blocks to be
# moved which is then recursed over until we hit a wall or can make
# the move

# expand our map
map <- in_map |> str_split_fixed("", n=Inf)
blocks <- t(t(which(map == "O", arr.ind = TRUE)) * c(1,2) - c(0,1)) |> as.data.frame()
guard  <- t(t(which(map == "@", arr.ind = TRUE)) * c(1,2) - c(0,1)) |> as.data.frame()

walls  <- t(map == "#") |> rep(each=2) |>
  matrix(nrow=nrow(map), byrow=TRUE)

map_list <-
  list(guard = guard |> mutate(col2 = col),
       blocks = blocks |> mutate(col2 = col+1))

do_move2 <- function(map_list, move) {

  # check for a hit wall
  hit_wall <- function(positions) {
    any(walls[positions[,1:2,drop=FALSE]] |
          walls[positions[,c(1,3),drop=FALSE]])
  }

  # move the guard
  next_guard <- map_list$guard + c(move, move[2])
  if (hit_wall(next_guard |> as.matrix()))
    return(map_list)

  # states for finding next blocks
  positions    <- next_guard
  blocks_left  <- map_list$blocks
  blocks_moved <- map_list$blocks |> slice(0)

  while(nrow(positions) > 0) {
    # if we hit a block we need to move that too
    hit_blocks <- blocks_left |> semi_join(positions,
                                             join_by(row, overlaps(x$col, x$col2, y$col, y$col2)))
    blocks_left <- blocks_left |> anti_join(positions,
                            join_by(row, overlaps(x$col, x$col2, y$col, y$col2)))
    if (nrow(hit_blocks) == 0)
      break # no block hit -> done!
    # move these blocks as well
    positions = hit_blocks + matrix(1, ncol=1, nrow=nrow(hit_blocks)) %*% c(move, move[2])
    blocks_moved <- rbind(blocks_moved, positions)
  }
  # now we know which blocks have moved, just need to check we didn't hit a wall
  hit_wall <- function(positions) {
    any(walls[positions[,1:2,drop=FALSE]] |
          walls[positions[,c(1,3),drop=FALSE]])
  }
  if (hit_wall(blocks_moved |> as.matrix()))
    return(map_list)

  # if we get here we're done, just need to update our map list
  new_map <- map_list
  new_map$guard <- next_guard
  new_map$blocks <- bind_rows(blocks_moved, blocks_left)
  return (new_map)
}

to_map <- function(map_list) {
  map <- matrix('.', nrow=nrow(walls), ncol=ncol(walls))
  map[walls] <- "#"
  map[map_list$guard[,1:2] |> as.matrix()] <- "@"
  map[map_list$blocks[,1:2] |> as.matrix()] <- "["
  map[map_list$blocks[,c(1,3)] |> as.matrix()] <- "]"
  map
}

dump_map <- function(map_list) {
  map_list |> to_map() |> apply(1, \(x) paste0(x, collapse='')) |> as.matrix(nrow=nrow(map)) |> print()
}

dump_map(map_list)

# ok, move our guard/blocks
map_moved <- map_list
for (i in seq_along(moves)) {
  map_moved <- do_move2(map_moved, moves[[i]])
}

gps2 <- function(map) {
  sum((which(map == "[", arr.ind = TRUE) - 1) %*% c(100,1))
}

gps2(map_moved |> to_map())
