library(tidyverse)

jets <- '>>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>' |>
  str_split('') |> unlist()

jets <- readLines('2022/day17/input.txt') |>
  str_split('') |> unlist()

# ok, now we drop rocks in cycles.
rocks <- list(matrix(TRUE, nrow=1, ncol=4),
              matrix(c(FALSE, TRUE, FALSE, TRUE, TRUE, TRUE, FALSE, TRUE, FALSE), nrow=3, ncol=3),
              matrix(c(TRUE, TRUE, TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, TRUE), nrow=3, ncol=3, byrow=TRUE),
              matrix(TRUE, nrow=4, ncol=1),
              matrix(TRUE, nrow=2, ncol=2))

jet_dir <- c('>' = 1, '<' = -1)

# iterate
jet_cycle <- c()
jet_cycle_i <- c()

tower_height <- function(tower) {
  max(0, apply(tower, 2, \(x) max(which(x))))
}

drop_rocks <- function(num_iter) {

  tower <- matrix(FALSE, ncol=7, nrow=round(13*num_iter/5))

  max_height = tower_height(tower)
  cur_jet = 0
  cur_rock = 0
  for (i in 1:num_iter) {
    # next rock
    cur_rock = cur_rock %% length(rocks) + 1
    # position 3 above the top of our tower
    rock = rocks[[cur_rock]]
    rock_posY = max_height + 4
    rock_posX = 3
    while (TRUE) {
      # next jet
      if (cur_jet == length(jets)) {
        cat('jets have cycled at maxheight', max_height, 'i=', i, '\n')
        jet_cycle <<- c(jet_cycle, max_height)
        jet_cycle_i <<- c(jet_cycle_i, i)
      }
      cur_jet = cur_jet %% length(jets) + 1
      # push the rock with the jet
      next_posX = rock_posX + jet_dir[jets[[cur_jet]]]
      if (next_posX < 1) next_posX = 1
      if (next_posX > 8 - ncol(rock)) next_posX = 8 - ncol(rock)
      # check if it intersects with our tower. If it does, don't move it
      if (!any(rock & tower[rock_posY + 1:nrow(rock)-1, next_posX + 1:ncol(rock)-1]))
        rock_posX = next_posX
      # move the rock down
      next_posY = rock_posY - 1
      # check if we've hit something on the downward move
      if (next_posY < 1 || any(rock & tower[next_posY + 1:nrow(rock)-1, rock_posX + 1:ncol(rock)-1])) {
        # we've run into something, stop the rock!
        tower[rock_posY + 1:nrow(rock)-1, rock_posX + 1:ncol(rock)-1] =
          tower[rock_posY + 1:nrow(rock)-1, rock_posX + 1:ncol(rock)-1] | rock
        max_height = tower_height(tower)
        break
      }
      rock_posY = next_posY
    }
  }
  tower[1:max_height,]
}

tower <- drop_rocks(2022)
tower_height(tower)

# part 2 will have cycles as clearly can't iterate.
# note that:
length(jets)
length(rocks)

# are both primes, so probably there'll be a cycle
# to do with this maybe?
jet_cycle <- c()
jet_cycle_i <- c()
tower <- drop_rocks(10000)

jet_cycle_i
diff(jet_cycle_i)
jet_cycle
diff(jet_cycle)

# ok, so jets cycle every i=1735 rocks starting at
# i = 1729 with our rocks cycling as well.

# this is potentially when things can repeat. Let's check:
all.equal(tower[jet_cycle[1]:jet_cycle[2],],
          tower[jet_cycle[2]:jet_cycle[3],])

# After jet_cycle_i[1] we're at height jet_cycle[1]
# Then every 1735 after that we increase our height
# by 2720.

# so height after X rocks should be the same as height
# after (X - 1729) %% 1735 + 1729

(1000000000000 - 1729) %% 1735 + 1729

# i.e. after 1875 cycles. Run for 1875 rocks...
tower <- drop_rocks(1875)
tower_height(tower)

# so we have:
((1000000000000 - 1729) %/% 1735)*2720 + 2929
