library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines("2023/day11/input.txt") |>
  str_split_fixed('', n=Inf)

# expand the galaxy
exp_rows <- rep(1:nrow(input), times=
      apply(input, 1, \(x) all(x == ".")) + 1)

exp_cols <- rep(1:nrow(input), times=
                  apply(input, 2, \(x) all(x == ".")) + 1)

galaxy <- input[exp_rows,exp_cols]

# ok, now find our galaxies
which(galaxy == "#", arr.ind = TRUE) |>
  dist(method='manhattan') |>
  sum()

# part 2: I suspected this: now we need to expand our galaxies
#         by way more space. It's not possible to just do this
#.        with the above trick as ofc we're going to end up
#         with a matrix over a million by a million.

#.        BUT: the manhattan index decomposes, right? So actually
#         we can still do the expansion trick directly, just
#         have to do it on the rows-first or cols-first ideas

gal_pos <- which(input == "#", arr.ind=TRUE)

x <- gal_pos[,1] |> unique() |> sort()
y <- gal_pos[,2] |> unique() |> sort()

nx <- tibble(pos=x) |>
  mutate(dx = pos - lag(pos, default=0) - 1) |>
  mutate(offset = 1000000*dx+1) |>
  mutate(newpos = cumsum(offset)) |>
  select(x=pos, nx=newpos)

ny <- tibble(pos=y) |>
  mutate(dx = pos - lag(pos, default=0) - 1) |>
  mutate(offset = 1000000*dx+1) |>
  mutate(newpos = cumsum(offset)) |>
  select(y=pos, ny=newpos)

gal_pos |>
  as.data.frame() |>
  set_names(c('x', 'y')) |>
  left_join(nx) |>
  left_join(ny) |>
  select(nx, ny) |>
  dist(method='manhattan') |>
  sum()

