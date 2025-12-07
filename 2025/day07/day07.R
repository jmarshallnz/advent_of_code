library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

raw <- ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
..............." |>
  str_split("\\n") |> unlist() |>
  str_split_fixed('', n=Inf)

raw <- readLines("2025/day07/input.txt") |>
  str_split_fixed('', n=Inf)

beams <- raw[1,] == "S"

splits <- 0
for (i in 2:nrow(raw)) {
  check <- beams & raw[i,] == "^"
  if (any(check)) {
    # split the beams
    beams[c(which(check) - 1,
            which(check) + 1)] <- TRUE
    beams[check] <- FALSE
    splits <- splits + sum(check)
  }
}
splits

# part 2: we need to count the possible different beam timelines.
#         the trick is that at each split, the number of timelines accumulate (from beams
#         uniting back together) so the number of beam timelines at position j is the
#         number at position j-1 plus the number at j+1 if there is a split
beams <- (raw[1,] == "S") |> as.numeric()

splits <- 0
for (i in 2:nrow(raw)) {
  check <- (beams > 0) & raw[i,] == "^"
  if (any(check)) {
    # split the beams
    beams[which(check) - 1] <- beams[which(check) - 1] + beams[which(check)]
    beams[which(check) + 1] <- beams[which(check) + 1] + beams[which(check)]
    beams[check] <- 0
  }
}
sum(beams)
