library(tidyverse)

d <- readLines("day8/input.txt") |>
  str_split_fixed("", n=Inf)

# find the antenna - anything matching a digit or character

loc <- which(d %in% c(0:9, LETTERS, letters))
antenna <- tibble(loc = loc, wch = d[loc]) |>
  mutate(ind = arrayInd(loc, .dim=dim(d), .dimnames=c('x', 'y'), useNames=TRUE) |> as.data.frame()) |>
  unnest(ind) |> rowid_to_column()

# hotspots are those the same distance from pairs of the same antenna.
# or, we could compute based on the pair-wise distance
antenna |> left_join(antenna, by="wch", relationship='many-to-many') |>
  filter(rowid.x != rowid.y) |>
  mutate(dx = row.y - row.x, dy = col.y - col.x) |>
  mutate(row.hs = row.y + dx, col.hs = col.y + dy) |>
  filter(row.hs > 0, row.hs <= nrow(d),
         col.hs > 0, col.hs <= ncol(d)) |>
  select(row.hs, col.hs) |>
  unique() |>
  nrow()

# part 2. Hotspots are k * (dx,dy) for integers k. Seems we won't need more than
# max(dim(d))-1 of them
max_rep <- max(dim(d))-1
integers <- tibble(offset=0:max_rep)
antenna |> left_join(antenna, by="wch", relationship='many-to-many') |>
  filter(rowid.x != rowid.y) |>
  mutate(dx = row.y - row.x, dy = col.y - col.x) |>
  cross_join(integers) |>
  mutate(row.hs = row.y + offset*dx, col.hs = col.y + offset*dy) |>
  filter(row.hs > 0, row.hs <= nrow(d),
         col.hs > 0, col.hs <= ncol(d)) |>
  select(row.hs, col.hs) |>
  unique() |>
  nrow()

