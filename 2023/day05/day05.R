library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines("2023/day05/input.txt")

seeds <- read.table(text=input[1], header=FALSE)[,-1] |>
  as.numeric()

breaks <- str_detect(input, "map:") |>
  cumsum()

maps <- data.frame(map = breaks[-1], data = input[-1]) |>
  filter(data != "", !str_detect(data, "map:")) |>
  extract(data, into=c("dest", "source", "length"), regex="([0-9]+) ([0-9]+) ([0-9]+)", convert=TRUE) |>
  nest(data=c("dest", "source", "length")) |>
  pull(data)

# ok, now we split the maps and have a function to apply them
map_ids <- function(ids, map) {
  tibble(id = ids) |>
    left_join(map |> mutate(source_right = source+length),
              by=join_by(id >= source, id <= source_right)) |>
    replace_na(list(dest = 1, source=1)) |>
    mutate(dest_id = dest + id-source) |>
    pull(dest_id)
}

reduce(maps, map_ids, .init = seeds) |>
  min()

# Part 2: OK, we can't use the above directly. Instead we need to have some idea of the breaks
# It's just the application of a series of piece-wise linear equations.
# applying a piece-wise linear equation twice just gives another piece-wise linear equation.
# we can define these basically by their split-points.
# so we need a way to take the existing maps and compose them into a piece-wise linear thing
# that can then be composed again etc.

maps[[1]] %>% mutate(dest_right = dest + length) |>
  left_join(maps[[2]] %>% mutate(source_right = source+length),
            by = join_by(dest >= source, dest <= source_right))


fill_in_missing_bits <- function(map) {
  missing_bits <- map |>
    arrange(source) |>
    mutate(lagged = lag(source+length, default = 0)) |>
    filter(source != lagged) |>
    mutate(length = source-lagged, dest = lagged, source = lagged) |>
    select(dest, source, length)
  bind_rows(map, missing_bits)
}

maps <- map(maps, fill_in_missing_bits)

# OK, now our maps are complete, i.e. they're basically permutations of the domain space.
# we can now I think work out how to apply them in succession

dest    source    length
<dbl>     <dbl>     <dbl>
1         0 205577967   4476015
2   4476015 622435882  12814017
3  17290032 210053982  19117526
4  36407558 487541414   6411511
5  42819069 472405294  15136120


dest    source    length
<dbl>     <dbl>     <dbl>
1  645569668         0 375136940
2  398047045 375136940 121772574
3  519819619 496909514 125750049
4  146677673 622659563  93167301
5  362307857 715826864  35739188
6          0 751566052  77735382

merge_map <- function(curr, map) {
  dest = c(curr |> pull(dest), map |> pull(source)) |> sort() |> unique()
  newdest <- tibble(dest = dest)

  extend_curr <- newdest |> left_join(curr) |>
    mutate(newlength = lead(dest, default=2^32) - dest) |>
    mutate(newlength = if_else(is.na(newlength), length, newlength)) |>
    fill(source) |>
    group_by(source) |>
    mutate(cumlen = cumsum(newlength)) |>
    ungroup() |>
    mutate(newsource = if_else(is.na(length), lag(source) + lag(cumlen), source)) |>
    select(dest, source = newsource, length = newlength, os=source, ol=length)

  merged_map <- extend_curr |> mutate(dest_right = dest+length) |>
    left_join(map |> mutate(source_right = source+length),
              join_by(dest >= source, dest_right <= source_right))
    rename(source = source.x, length=length.x) |>
    mutate(newdest = dest.x-source.y+dest.y) |>
    select(dest = newdest, source, length)

  return(merged_map)
}

3502926371   1436713
3502926371.  1267853

3504363084

  print(n=100)

  668867483+77735382

fill_source(source, length) {
  for (i in 1:length(source))
  if (is.na(source[i])) {
    source[i] + cumsum(source)
  }
}

maps2 <- maps[-1]

maps2[[1]] <- merge_map(maps[[1]], maps[[2]])

reduce(maps2, map_ids, .init = seeds) |>
  min()

reduce(maps, map_ids, .init = seeds) |>
  min()

single_map <- reduce(maps, merge_map)
map_ids(seeds, single_map) |> min()

# ok, we have our single map. Now merge this with our seeds range
seeds_range <- t(matrix(seeds, nrow=2)) |> as.data.frame() |>
  set_names(nm = c("source", "length"))

extended_range <- seeds_range |>
  bind_rows(seeds_range |> mutate(source = source+length) |> slice_max(source) |> mutate(length = 2^32-source))

extended_range <- extended_range |>
  mutate(dest = source) |>
  fill_in_missing_bits()

extended_range |> arrange(dest) |> mutate(lagged = lag(dest + length, default=0)) |>
  filter(dest != lagged)

merged <- merge_map(extended_range, single_map) |>
  print(n=300)

seeds_range |>
  mutate(source_max = source+length) |>
  left_join(merged |> mutate(source_right = source+length),
            by=join_by(x$source <= y$source, x$source_max >= y$source_right)) |>
  slice_min(dest, n=1) |>
  pull(dest)
