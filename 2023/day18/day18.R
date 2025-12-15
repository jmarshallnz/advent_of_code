library(tidyverse)
library(collections)

options(digits = 22,
        pillar.max_dec_width = 22)

directions <- read.table('2023/day18/input.txt', comment.char = '') |>
  set_names(c('dir', 'length', 'color'))

# Part 1 is the same as Day 10 I think?

# i.e. do the path, noting which is on one side.
# then flood fill.

# first find out how big the grid will be
dx <- c('D' = 0, 'U' = 0, 'L' = -1, 'R' = 1)
dy <- c('D' = 1, 'U' = -1, 'L' = 0, 'R' = 0)
right <- tribble(~dir, ~rx, ~ry,
                'D', -1, 0,
                'U', 1, 0,
                'L', 0, -1,
                'R', 0, 1)

dxy <- dx |> enframe(name='dir', value='dx') |> left_join(dy |> enframe(name='dir', value='dy'))
directions |>
  left_join(dxy) |>
  mutate(xoff = cumsum(dx*length), yoff = cumsum(dy*length)) |>
  reframe(range(xoff), range(yoff))

# OK, so if we go with say +240, +360 we'll be right
pos <- list(row=360, col=240)

# now plot out our path, noting which side is which
do_left_path <- function(directions, pos) {
  directions |>
    mutate(seq = map(length, \(x) 1:x)) |>
    unnest(seq) |>
    left_join(dxy) |>
    mutate(row = cumsum(dy) + pos$row,
           col = cumsum(dx) + pos$col) |>
    left_join(right) |>
    mutate(rrow = row + ry, rcol = col + rx)
}

map_fill <- do_left_path(directions, pos)

# ok, now fill our map to check
map <- matrix(0L, nrow=400, ncol=500)
map[map_fill |> select(rrow, rcol) |> as.matrix()] <- 2 # left
map[map_fill |> select(row, col) |> as.matrix()] <- 1 # inside

image(map)
image(map[100:250,100:250])

flood_fill <- function(path, left, iterate=100) {
  # neighbours
  nb <- tribble(~dx, ~dy,
                -1, 0,
                1, 0,
                0, -1,
                0, 1)
  left <- left |> anti_join(path)

  count <- 1
  while(TRUE) {
    path <- bind_rows(path, left) |> unique()
    left_nb <- left |> cross_join(nb) |>
      mutate(row = row + dy,
             col = col + dx, .keep ='unused') |>
      unique() |>
      anti_join(left) |>
      anti_join(path)
    cat("another ", nrow(left_nb), "neighbours\n")
    if (nrow(left_nb) == 0)
      break
    left <- left_nb
    count <- count + 1
    if (count > iterate)
      break
  }
  path
}

path <- map_fill |> select(row, col) |> unique()
left <- map_fill |> select(row = rrow, col = rcol) |> unique()

path <- flood_fill(path, left)

# check:
map <- matrix(0L, nrow=400, ncol=500)
map[path |> as.matrix()] <- 1L
image(map)

path |> unique() |> nrow()

# OK, for part 2 we can't just do the above. Instead we need
# to figure out the unique x/y and do it in BLOCKS instead.
# the same basic grid idea will work, but now each of the
# map rows/cols have different width.
# this should just be a matter of defining those and then
# redefining our positions on that refined grid

big_dir <- directions |> extract(color, into=c("hexlen", "dir"), regex="([0-9a-f]{5})([0-3])", convert=TRUE) |>
  mutate(length = strtoi(hexlen, base=16)) |>
  mutate(dir = c('R', 'D', 'L', 'U')[dir+1]) |>
  select(dir, length)

big_pos <- big_dir |>
  left_join(dxy) |>
  mutate(x = cumsum(dx*length), y = cumsum(dy*length))

# OK, so the idea is that these rows and columns are still 1m wide
# but the gaps between them are wider!

# and the size of the grid is then the dx/dy.
grid_col <- big_pos |> select(x) |> unique() |> arrange(x) |>
  mutate(gap = x+1) |>
  pivot_longer(everything(), values_to='x') |>
  select(-name) |>
  tibble::rowid_to_column('col') |>
  mutate(width = lead(x, default=max(x)) - x)

grid_row <- big_pos |> select(y) |> unique() |> arrange(y) |>
  mutate(gap = y+1) |>
  pivot_longer(everything(), values_to='y') |>
  select(-name) |>
  tibble::rowid_to_column('row') |>
  mutate(height = lead(y, default=max(y)+0) - y)

grid <- expand_grid(grid_col, grid_row)

# OK, now redefine our lengths accordingly
ox <- grid_col |> filter(x == 0) |> pull(col)
oy <- grid_row |> filter(y == 0) |> pull(row)
directions <- big_pos |> left_join(grid, by=c('x', 'y')) |>
  mutate(len = (col - lag(col, default=ox))*dx +
               (row - lag(row, default=oy))*dy) |>
  select(dir, length=len, row, col)

pos <- list(row=oy, col=ox)

# ok, same as above now
map_fill <- do_left_path(directions, pos)

path <- map_fill |> select(row, col) |> unique()
left <- map_fill |> select(row = rrow, col = rcol) |> unique()

path <- flood_fill(path, left, iterate=200)

map <- matrix(0L, nrow=nrow(grid_row), ncol=nrow(grid_col))
map[path |> as.matrix()] <- 1
image(map)

# OK, compute area
path |> left_join(grid) |>
  summarise(sum(width*height))
