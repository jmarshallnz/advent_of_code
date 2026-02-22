library(tidyverse)
library(collections)
library(hashmap)

input <- '....#..
..###.#
#...#.#
.#...##
#.###..
##.#.##
.#..#..' |>
str_split('\n') |> unlist()

input <- readLines('2022/day23/input.txt')

map <- input |> str_split_fixed('', n=Inf)

# OK, go and run a loop 10 times to update the elve positions.
# This is a O(n^2) algorithm, as for each elf we need to check
# the other elve positions. For Part 1 this isn't really a problem
# as there's only 2.4k elves, so the linear lookup isn't a problem
dirs <- data.frame(dir=c('N', 'S', 'W', 'E'), prior=1:4)
offs <- list(N = expand.grid(x=-1, y=-1:1, mx=-1, my=0),
             S = expand.grid(x=1, y=-1:1, mx=1, my=0),
             W = expand.grid(x=-1:1, y=-1, mx=0, my=-1),
             E = expand.grid(x=-1:1, y=1, mx=0, my=1)) |>
  bind_rows(.id = 'dir')

# OK, iterate around, starting with N
elves <- which(map == "#", arr.ind = TRUE) |>
  as.data.frame() |> rowid_to_column('elf')

max_moves = 1000
for (move in 1:max_moves) {
  # for each elf, first check if there's another elf in any direction
  # then check
  proposed_elves <- elves |> cross_join(offs) |>
    mutate(row = row + x, col = col+y, .keep = 'unused') |>
    anti_join(elves, by=c('row', 'col')) |>
    count(elf, dir, mx, my) |>
    group_by(elf) |>
    filter(n == 3, sum(n) < 12) |> # an elf on the horizon
    left_join(dirs, by='dir') |>
    slice_min(prior, n=1, with_ties = FALSE) |>
    ungroup() # priority direction

  # They can only move if their new positions are unique
  moveable_elves <- proposed_elves |> left_join(elves, by='elf') |>
    mutate(row = row + mx, col = col + my) |>
    add_count(row, col, name='rc') |>
    filter(rc == 1) |>
    select(elf, new_row = row, new_col = col)

  cat("at move", move, "we can move", moveable_elves |> nrow(), "elves\n")
  if (moveable_elves |> nrow() == 0)
    break

  # OK, move these elves
  elves <- elves |> left_join(moveable_elves, by='elf') |>
    mutate(row = if_else(is.na(new_row), row, new_row),
           col = if_else(is.na(new_col), col, new_col)) |>
    select(elf, row, col)
  # check stuff
  if (move %% 20 == 0) {
    im <- matrix(0, nrow=diff(range(elves$row))+1, ncol=diff(range(elves$col))+1)
    im[elves |> mutate(row = row - min(row) + 1, col = col-min(col)+1) |> select(row, col) |> as.matrix()] <- 1
    image(t(im[nrow(im):1,]))
  }
  # and rotate our direction
  dirs <- dirs |> mutate(prior = lag(prior, default=prior[4]))
}

elves |> summarise(rows = diff(range(row))+1,
                   cols = diff(range(col))+1,
                   score = rows*cols - n())
