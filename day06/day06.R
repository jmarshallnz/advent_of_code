library(tidyverse)

input <- read.table(text="....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...", comment.char='')

input <- read.table("day6/input.txt", comment.char='')

map <- input |>
  pull(V1) |> str_split_fixed('', n=Inf)

guard <- which(map == "^", arr.ind=TRUE)
dir <- t(c(-1,0))

# Hmm, is there an elegant way to do this? Not that comes to mind. Maybe
# some graph algorithm? e.g. using coloured edges for direction, and
# change colour at special nodes?

turn_right <- matrix(c(0, -1, 1, 0), nrow=2, byrow = TRUE)

# otherwise just iterate I guess...
route <- map
while (TRUE) {
  route[guard] <- "X"
  guard_next <- guard+dir
  if (any(guard_next < 1) |
      any(guard_next > c(nrow(map), ncol(map)))) {
    break # finished
  }
  if (map[guard_next] == "#") { # turn right
    dir = dir%*%turn_right
  } else { # move forward
    guard <- guard_next
  }
}

sum(route == "X")

# Part 2 means we must create a cycle. I think this calls for graph theory...
# But how do we generate the correct graph?

# One idea I think is to hookup the obstacles as being adjacent if a turn
# from one results in hitting the next. This gives a directed graph. You then
# add obstacles and check if the directed graph has a cycle (and that the
# guard will hit it I guess?)

# This still seems clunky. I wonder if there's a condition on the rows and
# columns that must be in the loop that could be used to filter things down

# Instead, I think we'll just do it the dumb way!
# Just iterate across the region for "new obstacles"
# and then check for cycles in the above loop.

# One speedup is recognising that the obstacles can only be on the existing
# guard walk from part 1 I think? Otherwise the guard won't hit them.

# we need to store the previous directions we've seen the guard
# go so we can detect a cycle.

dir_store <- matrix(c(0,1,0,2,0,3,0,4,0), nrow=3, byrow=TRUE) # map of dir -> 1:4
guard_cycles <- function(map, guard, dir, obs=NULL) {
  prev_dirs <- array(0, dim=c(nrow(map), ncol(map), 4)) # previous possible directions

  cat("checking obs=", obs, "\n")
  if (!is.null(obs)) {
    map[obs] <- "#"
#    print(map)
  }
  while (TRUE) {
    if (map[guard] == "X" && prev_dirs[cbind(guard, dir_store[dir+c(2,2)])]) {
      # we've been here before in the same direction ->
      return(TRUE)
    }
    #if (map[guard] == "X") {
#      print(map)
  #    cat("been at ", guard, " here before in direction", prev_dirs[guard], "\n")
   #   cat("current direction is", dir, " or ", dir_store[dir + c(2,2)], "\n")
    #}
    map[guard] <- "X"
    prev_dirs[cbind(guard, dir_store[dir+c(2,2)])] = TRUE
    guard_next <- guard+dir
    if (any(guard_next < 1) |
        any(guard_next > c(nrow(map), ncol(map)))) {
      break # finished
    }
    if (map[guard_next] == "#") { # turn right
      dir = dir%*%turn_right
    } else { # move forward
      guard <- guard_next
    }
  }
  return(FALSE) # no cycle
}

# now, let's place obstacles and see if he escapes
obs <- which(route == "X", arr.ind=TRUE) |>
  as_tibble()
guard <- which(map == "^", arr.ind = TRUE)

# do computation
obs |>
  mutate(cycles = map2_lgl(row, col, ~ guard_cycles(map, guard, dir, obs=matrix(c(.x, .y),
                                                                            nrow=1)))) |>
  summarise(sum(cycles))

