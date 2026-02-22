library(tidyverse)

options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines('2020/day17/input.txt') |>
  str_split_fixed('', n=Inf) |>
  apply(2, \(x) x == '#')

# OK, so we need to expand our map to a 3D array of 1 more size all around
do_step <- function(grid) {
  # expand our current grid out by two so that we don't need to care about
  # boundary cases
  dg1 <- dim(grid)[1]; dg2 <- dim(grid)[2]; dg3 <- dim(grid)[3]
  exp_grid <- array(0, dim=c(dg1+4, dg2+4, dg3+4))
  exp_grid[seq_len(dg1)+2,
           seq_len(dg2)+2,
           seq_len(dg3)+2] <- grid

  # OK, now our next_grid
  # If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active.
  # Otherwise, the cube becomes inactive.
  # If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active.
  # Otherwise, the cube remains inactive.
  # add up the number of active neighbours for each grid item
  active <- array(0, dim=c(dg1+2, dg2+2, dg3+2))
  for (x1 in -1:1) {
    for (x2 in -1:1) {
      for (x3 in -1:1) {
        if (x1 != 0 || x2 != 0 || x3 != 0) {
          active <- active + exp_grid[seq_len(dg1+2)+1+x1,seq_len(dg2+2)+1+x2,seq_len(dg3+2)+1+x3,drop=FALSE]
        }
      }
    }
  }
  # OK, so any active cube where the neighbours are 2 or 3 are inactive
  out <- array(0, dim=dim(active))
  out <- ((active == 2 | active == 3) & exp_grid[seq_len(dg1+2)+1,
                                                seq_len(dg2+2)+1,
                                                seq_len(dg3+2)+1]) | # was active, is still active
    ((active == 3) & !exp_grid[seq_len(dg1+2)+1,
                              seq_len(dg2+2)+1,
                              seq_len(dg3+2)+1]) # was inactive, now active
  return(out)
}

grid <- array(input, dim=c(nrow(input), ncol(input), 1))
for (i in 1:6) {
  grid <- do_step(grid)
}

# Part 2: Same thing but now we're in 4D instead

# OK, so we need to expand our map to a 3D array of 1 more size all around
do_step2 <- function(grid) {
  # expand our current grid out by two so that we don't need to care about
  # boundary cases
  dg1 <- dim(grid)[1]; dg2 <- dim(grid)[2]; dg3 <- dim(grid)[3]; dg4 <- dim(grid)[4]
  exp_grid <- array(0, dim=c(dg1+4, dg2+4, dg3+4, dg4+4))
  exp_grid[seq_len(dg1)+2,
           seq_len(dg2)+2,
           seq_len(dg3)+2,
           seq_len(dg4)+2] <- grid

  # OK, now our next_grid
  # If a cube is active and exactly 2 or 3 of its neighbors are also active, the cube remains active.
  # Otherwise, the cube becomes inactive.
  # If a cube is inactive but exactly 3 of its neighbors are active, the cube becomes active.
  # Otherwise, the cube remains inactive.
  # add up the number of active neighbours for each grid item
  active <- array(0, dim=c(dg1+2, dg2+2, dg3+2, dg4+2))
  for (x1 in -1:1) {
    for (x2 in -1:1) {
      for (x3 in -1:1) {
        for (x4 in -1:1) {
          if (x1 != 0 || x2 != 0 || x3 != 0 || x4 != 0) {
            active <- active + exp_grid[seq_len(dg1+2)+1+x1,seq_len(dg2+2)+1+x2,seq_len(dg3+2)+1+x3,
                                        seq_len(dg4+2)+1+x4,drop=FALSE]
          }
        }
      }
    }
  }
  # OK, so any active cube where the neighbours are 2 or 3 are inactive
  out <- array(0, dim=dim(active))
  out <- ((active == 2 | active == 3) & exp_grid[seq_len(dg1+2)+1,
                                                 seq_len(dg2+2)+1,
                                                 seq_len(dg3+2)+1,
                                                 seq_len(dg4+2)+1]) | # was active, is still active
    ((active == 3) & !exp_grid[seq_len(dg1+2)+1,
                               seq_len(dg2+2)+1,
                               seq_len(dg3+2)+1,
                               seq_len(dg4+2)+1]) # was inactive, now active
  return(out)
}

grid <- array(input, dim=c(nrow(input), ncol(input), 1, 1))
for (i in 1:6) {
  grid <- do_step2(grid)
}

