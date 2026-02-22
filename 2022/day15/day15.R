library(tidyverse)

options(digits = 22,
        pillar.max_dec_width = 22)

input <- "Sensor at x=2, y=18: closest beacon is at x=-2, y=15
Sensor at x=9, y=16: closest beacon is at x=10, y=16
Sensor at x=13, y=2: closest beacon is at x=15, y=3
Sensor at x=12, y=14: closest beacon is at x=10, y=16
Sensor at x=10, y=20: closest beacon is at x=10, y=16
Sensor at x=14, y=17: closest beacon is at x=10, y=16
Sensor at x=8, y=7: closest beacon is at x=2, y=10
Sensor at x=2, y=0: closest beacon is at x=2, y=10
Sensor at x=0, y=11: closest beacon is at x=2, y=10
Sensor at x=20, y=14: closest beacon is at x=25, y=17
Sensor at x=17, y=20: closest beacon is at x=21, y=22
Sensor at x=16, y=7: closest beacon is at x=15, y=3
Sensor at x=14, y=3: closest beacon is at x=15, y=3
Sensor at x=20, y=1: closest beacon is at x=15, y=3" |>
  str_split("\n") |> unlist()

input <- readLines("2022/day15/input.txt")

input <- tibble(input = input) |>
  extract(input, into=c('sensor', 'beacon'), regex="(.*?):(.*)") |>
  extract(sensor, into=c('sx', 'sy'), regex="at x=([-0-9]+), y=([-0-9]+)", convert=TRUE) |>
  extract(beacon, into=c('bx', 'by'), regex="at x=([-0-9]+), y=([-0-9]+)", convert=TRUE) |>
  mutate(dist = abs(bx-sx) + abs(by-sy))

# we want bx such that abs(bx-sx) <= dist - abs(2000000-sy)
# we want bx such that abs(2000000-sy)-dist <= bx-sx <= dist - abs(2000000-sy)
# we want bx such that abs(2000000-sy)-dist+sx <= bx <= dist - abs(2000000-sy) + sx

row <- 2000000
spotted <- input |>
  filter(dist >= abs(row-sy)) |>
  mutate(low = abs(row-sy)-dist+sx, high = dist - abs(row-sy) + sx) #|>

spotted_range <- spotted |>
  summarise(mlow = min(low), mhigh=max(high))

# we want to union up the segments low,high. We can do this by intersecting
# the inverses I guess? i.e. < low or > high

# or can probably just do the dumb thing in this case as well I guess. Depends
# if this is gonna be used for part 2 or not?
all <- tibble(sx=seq(spotted_range$mlow, spotted_range$mhigh)) |>
  semi_join(spotted, by=join_by(sx >= low, sx <= high)) |>
  anti_join(spotted |> filter(by == row), by=join_by(sx == bx))

all |> nrow()

# part 2: sure enough, our == 2000000 is a single case in the range 0...4000000

# so part 2 will be 4000000 times slower if we use the above technique.

# but, we know there's only a single one, so we have EQUALITY. Well, sort of.
# we want to find the single block on the set of 'diamond+1's that is not
# inside a diamond.

# we could probably brute-force it?
# essentially we want to find the intersection of (x - bx_i, y - by_i) == d_i+1

# the number of blocks to check is:
input |> summarise(sum(dist*4))

# this is quite large I guess?!? We'd need to join that to each of the other
# distances and compute the centers.
get_diamond <- function(dist) {
  x = seq(-dist, dist)
  y1 = c(seq(0, dist), seq(dist-1, 0))
  y2 = -y1
  tibble(x = rep(x, 2), y = c(y1, y2))
}

max_row_col <- 4000000
outer <- input |>
  mutate(diamond = map(dist+1, get_diamond)) |>
  unnest(diamond) |>
  mutate(x = x+sx, y = y+sy) |>
  select(x,y) |>
  filter(x >= 0, x <= max_row_col,
         y >= 0, y <= max_row_col)

outer2 <- outer |>
  unique()

filter_points <- function(points, sensor) {
  points |>
    filter(abs(x - sensor$sx) + abs(y - sensor$sy) > sensor$dist)
}

sensors <- input |> select(sx, sy, dist) |> rowwise() |>
  group_split()

possibles = outer
for (i in 1:length(sensors)) {
  cat("done i=", i, "\n")
  possibles <- filter_points(possibles, sensors[[i]])
}
possibles |>
  mutate(tuning_freq = x*4000000 + y)
