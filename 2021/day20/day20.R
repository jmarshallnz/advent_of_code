library(tidyverse)

input <- readLines('2021/day20/input.txt')
input <- '..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..###..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#..#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#......#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#.....####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.......##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###' |> str_split('\n') |> unlist()

lut <- ifelse((input[1] |> str_split('') |> unlist()) == "#", 1, 0)

# by the looks the '0' in our input data is a #, so we're progressively switching
# the 'outside' from . -> # -> . over and over

outside <- ifelse('.' == '#', 1, 0)
inside <- input[-c(1:2)] |> str_split_fixed('', n=Inf) |>
  apply(2, \(x) ifelse(x == '#', 1, 0))

# OK we iterate over the inside, outside pair to give a new one
next_map <- function(inside, outside) {
  lu <- matrix(0, nrow=nrow(inside)+2, ncol=ncol(inside)+2)
  # expand our inside so we don't need to care about bounds. This
  # should make this fast matrix adds
  in_ex <- matrix(outside, nrow=nrow(inside)+4, ncol=ncol(inside)+4)
  in_ex[2+seq_len(nrow(inside)), 2+seq_len(ncol(inside))] <- inside
  # iterate backwards to get the right power
  power <- 1
  for (row in 3:1) {
    for (col in 3:1) {
      lu <- lu + power*in_ex[row-1+seq_len(nrow(lu)),col-1+seq_len(ncol(lu))]
      power <- power * 2
    }
  }
  out <- matrix(lut[lu+1], nrow=nrow(lu), ncol=ncol(lu))
}

next_inside <- inside
next_outside <- outside
for (i in 1:2) {
  next_inside <- next_map(next_inside, next_outside)
  next_outside <- lut[sum(2^(8:0)*rep(next_outside, 9)) + 1]
}
sum(next_inside)

# Part 2: repeat the above 50 times instead. Lucky we made it fast, eh?
next_inside <- inside
next_outside <- outside
for (i in 1:50) {
  next_inside <- next_map(next_inside, next_outside)
  next_outside <- lut[sum(2^(8:0)*rep(next_outside, 9)) + 1]
}
sum(next_inside)
