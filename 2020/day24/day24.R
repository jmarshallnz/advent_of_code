library(tidyverse)
library(collections)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- 'sesenwnenenewseeswwswswwnenewsewsw
neeenesenwnwwswnenewnwwsewnenwseswesw
seswneswswsenwwnwse
nwnwneseeswswnenewneswwnewseswneseene
swweswneswnenwsewnwneneseenw
eesenwseswswnenwswnwnwsewwnwsene
sewnenenenesenwsewnenwwwse
wenwwweseeeweswwwnwwe
wsweesenenewnwwnwsenewsenwwsesesenwne
neeswseenwwswnwswswnw
nenwswwsewswnenenewsenwsenwnesesenew
enewnwewneswsewnwswenweswnenwsenwsw
sweneswneswneneenwnewenewwneswswnese
swwesenesewenwneswnwwneseswwne
enesenwswwswneneswsenwnewswseenwsese
wnwnesenesenenwwnenwsewesewsesesew
nenewswnwewswnenesenwnesewesw
eneswnwswnwsenenwnwnwwseeswneewsenese
neswnwewnwnwseenwseesewsenwsweewe
wseweeenwnesenwwwswnew' |> str_split('\n') |> unlist()

input <- readLines('2020/day24/input.txt')

# we'll use axial coordinates so that north/south moves r
# and q runs northwest/southeast
#.      -r
#   +s  |. +q
#.   \  | /
#.
# we don't bother storing s as r+q+s=0 (essentially this is a cubic
# grid cut down to a plane)

offsets <- list(e = c(1, 0), # move in +q and -s. r stays constant
             w = c(-1, 0),   # move in -q and +s. r stays constant
             ne = c(1, -1),  # move in +q and -r. s stays constant
             nw = c(0, -1),  # move in -r and +s. q stays constant
             se = c(0, 1),   # move in +r and -s. q stats constant
             sw = c(-1, 1))  # move in -q and +r. s stays constant

# parse our input
do_moves <- function(moves) {
  moves <- str_split(moves, '') |> unlist()
  step <- 1
  pos <- c(0, 0)
  while(step <= length(moves)) {
    dir <- moves[step]
    if (dir %in% c('n', 's')) {
      step <- step+1
      dir <- paste0(dir, moves[step])
    }
#    cat('dir = ', dir, 'offset=', offsets[[dir]], '\n')
    pos <- pos + offsets[[dir]]
    step <- step+1
  }
  return(data.frame(q=pos[1], r=pos[2]))
}

black_tiles <- tibble(input) |>
  mutate(pos = map(input, do_moves)) |>
  unnest(pos) |>
  count(q, r) |>
  filter(n %% 2 == 1) |>
  select(q, r) |> mutate(q = q - min(q) + 1, r = r - min(r) + 1)

black_tiles |> nrow()

# Part 2: we now evolve these tiles according to various rules repeatedly

# ok, so we have our cube grid (axial grid) of the right size
grid <- matrix(0, nrow=max(black_tiles$q), ncol=max(black_tiles$r))
grid[black_tiles |> as.matrix()] <- 1

# ok, now evolve our grid
evolve_grid <- function(grid) {

  offset_mat <- list_transpose(offsets) |> bind_rows() |> as.matrix()
  # our grid can increase by 2 in q and r each time around
  out_grid <- matrix(0, nrow=nrow(grid)+2, ncol=ncol(grid)+2)
  # to not worry about edge cases, we wrap our input in another layer again
  exp_grid <- matrix(0, nrow=nrow(grid)+2*2, ncol=ncol(grid)+2*2)
  exp_grid[2+seq_len(nrow(grid)), 2+seq_len(ncol(grid))] <- grid

  # OK, now iterate over exp_grid
  for (q in 1:nrow(out_grid)) {
    for (r in 1:ncol(out_grid)) {
      # count how many neighbours are at
      num_black_nb <- sum(exp_grid[t(c(q+1, r+1) + offset_mat)])
      is_black <- exp_grid[q+1, r+1]
#      if (is_black) {
#        cat('is black?\n')
#      }
      if (is_black && num_black_nb %in% c(1,2)) {
        # stays black
        out_grid[q, r] <- 1
      }
      if (!is_black && num_black_nb == 2) {
        # becomes black
        out_grid[q, r] <- 1
      }
    }
  }
  return(out_grid)
}

curr <- grid
for (i in 1:100) {
  curr <- evolve_grid(curr)
}
curr |> sum()
