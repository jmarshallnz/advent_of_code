library(tidyverse)

input <- readLines('2021/day22/input.txt')

state_changes <- tibble(input = input) |>
  extract(input, into=c('state', 'x1', 'x2', 'y1', 'y2', 'z1', 'z2'),
          regex="([onf]+) x=([-0-9]+)\\.\\.([-0-9]+),y=([-0-9]+)\\.\\.([-0-9]+),z=([-0-9]+)\\.\\.([-0-9]+)",
          convert=TRUE)

# by the looks part 2 is likely to extend part 1's domain to basically infinite?
internal_states <- state_changes |>
  filter(!(x1 > 50 | x2 < -50 | y1 > 50 | y2 < -50 | z1 > 50 | z2 < -50))

# in part 1 we could just track all possibles (101^3 = ~10^6) and just
# turn them on/off directly. This would be really fast.

cubes <- array(0, dim=c(101, 101, 101))
for (i in 1:nrow(internal_states)) {
  s <- slice(internal_states, i)
  if (s$state == 'off') next
  cubes[s$x1:s$x2+51, s$y1:s$y2+51, s$z1:s$z2+51] <- ifelse(s$state == 'on', 1, 0)
}
sum(cubes)

# in part 2 this won't be possible most likely: instead we'll need to maintain
# a set of 'on/off' regions. Assuming they're always cubes, we can basically
# do intersections. An "on" will just add a cube, and potentially CUT some existing
# cubes up:

# CONFIRMED :)

#
# ----
# | -|---
# | |   |
# ---   |
#.  |.  |
#   -----
# when we intersect cubes, we basically cut the cube into 8 subcubes

# We need only track the 'on' cubes. An 'off' step would intersect any cubes
# breaking them up. In theory we can multiply the previous cubes by 8 each time
# so this grows really fast. In practice, it probably doesn't though?

# we need to combine all our 'on' cubes into an "ON" cube list.
# then we combine all our 'off' cubes into an "OFF" cube list.

# These two steps are cube addition: when we go to add a cube,
# we basically split the cube to be added (if needed!) based on
# intersections. So we're always just adding to our list and
# never removing from it.

# then we difference these lists. This differencing
# is done by breaking up the starter list: we never need
# to break up the 'to delete' list, right?

# Actually, we don't have to combine the 'off' cubes at all.
# applying an off cube basically replaces an existing cube
# with up to 7 others.


cube_intersects <- function(c1, c2) {
  # check if c1/c2 overlap
  x1 <- max(c1$x1, c2$x1); x2 <- min(c1$x2, c2$x2)
  y1 <- max(c1$y1, c2$y1); y2 <- min(c1$y2, c2$y2)
  z1 <- max(c1$z1, c2$z1); z2 <- min(c1$z2, c2$z2)
  return(x2 > x1 && y2 > y1 && z2 > z1)
}

cube_difference <- function(c1, c2) {
  # split c1 by c2, removing the intersection from c1
  # find the intersection of c1 and c2
  x1 <- max(c1$x1, c2$x1); x2 <- min(c1$x2, c2$x2)
  y1 <- max(c1$y1, c2$y1); y2 <- min(c1$y2, c2$y2)
  z1 <- max(c1$z1, c2$z1); z2 <- min(c1$z2, c2$z2)
  # split into 27 cubes (many will be empty)
  splitx = c(c1$x1, x1, x2, c1$x2)
  splity = c(c1$y1, y1, y2, c1$y2)
  splitz = c(c1$z1, z1, z2, c1$z2)
  out <- list()
  for (i in 1:3) {
    x1 = splitx[i]; x2 = splitx[i+1]
    for (j in 1:3) {
      y1 = splity[j]; y2 = splity[j+1]
      for (k in 1:3) {
        z1 = splitz[k]; z2 = splitz[k+1]
        if (x2 > x1 && y2 > y1 && z2 > z1 &&
           !(i == 2 && j == 2 && k == 2)) { # add all cubes except the pure intersection
          out[[length(out)+1]] = lst(x1, x2, y1, y2, z1, z2)
        }
      }
    }
  }
  return(out)
}

cube_volume <- function(cu) {
  (cu$x2 - cu$x1)*(cu$y2 - cu$y1)*(cu$z2-cu$z1)
}

cube_test <- list(x1 = 0, x2 = 1, y1 = 0, y2 = 1, z1 = 0, z2 = 2)
cube_test2 <- list(x1 = 0, x2 = 1, y1 = 0, y2 = 1, z1 = 0, z2 = 1)

cube_difference(cube_test, cube_test2) # hmm, do we need non-zero bounds here?
cube_difference(cube_test2, cube_test) # hmm, do we need non-zero bounds here?
cube_intersects(cube_test, cube_test2)
cube_intersects(cube_test2, cube_test)

add_cube <- function(cubes, cube) {
  # run through our cubes, intersecting them with the given cube
  cubes_to_add <- queue()
  cubes_to_add$push(cube)
  while(cubes_to_add$size() > 0) {
    cube <- cubes_to_add$pop()
  #  cat("adding cube", cube |> unlist(), "\n")
    intersects <- FALSE
    for (cu in cubes) {
      # see if this cube intersects
   #   cat('checking for intersection with', cu |> unlist(), '\n')
      if (cube_intersects(cu, cube)) {
        # We need to split 'cube' into 8 subcubes, adding all
        # but the one that intersects to cubes_to_add
     #   cat('cube', cube |> unlist(), 'intersects', cu |> unlist(), '\n')
        intersects = TRUE
        cube_splits <- cube_difference(cube, cu)
      #  if (length(cube_splits) > 0) {
      #    cat("cube", cube |> unlist(), "splits by", cu |> unlist(), "\n")
      #  }
        for (split in cube_splits) {
     #     cat("adding split", split  |> unlist(), "\n")
          cubes_to_add$push(split)
        }
        break; # we're done with cube
      }
    }
    if (!intersects) { # add the cube
#      cat("doesn't intersect, so add it directly\n")
      cubes[[length(cubes)+1]] <- cube
    }
  }
  return(cubes)
}

subtract_cube <- function(cubes, cube) {
  # run through our cubes, intersecting them with the given cube
  out <- list()
  for (cu in cubes) {
    # see if this cube intersects
#    cat('checking for intersection with', cu |> unlist(), '\n')
    if (cube_intersects(cu, cube)) {
      # We need to split cu into 8 subcubes, removing the one that fully intersections
      cube_splits <- cube_difference(cu, cube)
 #     if (length(cube_splits) > 0) {
#        cat("cube", cu |> unlist(), "splits by", cube |> unlist(), "\n")
#      }
      for (split in cube_splits) {
 #       cat("adding split", split  |> unlist(), "\n")
        out[[length(out)+1]] <- split
      }
    } else {
      # doesn't intersect, so just add it directly
      out[[length(out)+1]] <- cu
    }
  }
  return(out)
}

# ok, add our initial cube
cubies_on <- internal_states |> filter(state == 'on') |>
  select(x1, x2, y1, y2, z1, z2) |>
  mutate(x2 = x2+1, y2 = y2+1, z2 = z2 + 1) |> # for non-zero area
  rowwise() |> group_split() |>
  as.list() |> map(as.list)

# OK, now with our cubies, let's go and add them together
on_cubes <- reduce(cubies_on[-1], add_cube, .init=cubies_on[1])
map_dbl(on_cubes, cube_volume) |>
  sum() # correct!

cubies_off <- internal_states |> filter(state == 'off') |>
  select(x1, x2, y1, y2, z1, z2) |>
  mutate(x2 = x2+1, y2 = y2+1, z2 = z2 + 1) |> # for non-zero area
  rowwise() |> group_split() |>
  as.list() |> map(as.list)

done_cubes <- on_cubes
for (cu in cubies_off) {
  done_cubes <- subtract_cube(done_cubes, cu)
  cat("volume=", map_dbl(done_cubes, cube_volume) |>
        sum(), "\n")
}

# OK, the reason this doesn't work is that ORDER IS IMPORTANT!
# We must iterate in order, as otherwise a ON state later won't toggle
# back on again

state_list <- internal_states |>
  select(x1, x2, y1, y2, z1, z2, state) |>
  mutate(x2 = x2+1, y2 = y2+1, z2 = z2 + 1) |> # for non-zero area
  rowwise() |> group_split() |>
  map(as.list) |> as.list()

# OK, iterate through our state list
done_cubes <- state_list[1]
for (s in state_list[-1]) {
  if (s$state == 'on') {
    done_cubes <- add_cube(done_cubes, s)
  } else {
    done_cubes <- subtract_cube(done_cubes, s)
  }
}

# compute our final volume
map_dbl(done_cubes, cube_volume) |>
  sum() # YAY!

# Right, now part 2!
state_list <- state_changes |>
  select(x1, x2, y1, y2, z1, z2, state) |>
  mutate(x2 = x2+1, y2 = y2+1, z2 = z2 + 1) |> # for non-zero area
  rowwise() |> group_split() |>
  map(as.list) |> as.list()

# OK, iterate through our state list
done_cubes <- state_list[1]
for (s in state_list[-1]) {
  cat("processing state", s |> unlist(), "\n")
  if (s$state == 'on') {
    done_cubes <- add_cube(done_cubes, s)
  } else {
    done_cubes <- subtract_cube(done_cubes, s)
  }
}

# compute our final volume
map_dbl(done_cubes, cube_volume) |>
  sum() # YAY!


cubies[[2]]
c2 <- cubies[[1]]
c1 <- list(x1=-4, x2=34, y1=-15, y2=25, z1=16, z2=43)

c1 <- list(x1=-4,x2=17,y1=25,y2=37,z1=16,z2=28)
c2 <- list(x1=-4,x2=34,y1=25,y2=30,z1=16,z2=43)
cube_intersects(c1, c2)
cube_difference(c1, c2) |> unlist() == c1 # hmm, this shouldn't happen!
