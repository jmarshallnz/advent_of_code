library(tidyverse)

rocks <- '498,4 -> 498,6 -> 496,6
503,4 -> 502,4 -> 502,9 -> 494,9' |>
  str_split("\n") |> unlist() |>
  str_split(" -> ") |> map(\(x) str_split_fixed(x, ',', n=2) |> as.data.frame())

rocks <- readLines("2022/day14/input.txt") |>
  str_split(" -> ") |> map(\(x) str_split_fixed(x, ',', n=2) |> as.data.frame())

fill_map <- function(rock) {
  filled <- list()
  rock <- rock |> mutate(across(everything(), as.numeric))
  for (i in 1:(nrow(rock)-1)) {
    if (rock[i, 1] == rock[i+1, 1]) {
      filled[[i]] <- data.frame(x=rock[i,1], y=seq(rock[i,2], rock[i+1, 2]))
    } else {
      filled[[i]] <- data.frame(x=seq(rock[i,1], rock[i+1, 1]), y=rock[i, 2])
    }
  }
  bind_rows(filled)
}

fill_this <- map_dfr(rocks, fill_map)

# ok, make a map grid
minX = min(fill_this$x, 500) - 1
minY = min(fill_this$y, 0) - 1

sandX = 500 - minX
sandY = 0 - minY

rock_pos <- fill_this |>
  mutate(x = x - minX, y = y - minY) |>
  select(x, y) |>
  as.matrix()

rock_pos

map <- matrix(0, nrow=max(rock_pos[,1]), ncol=max(rock_pos[,2]))
map[rock_pos] <- 1

image(map[,ncol(map):1])

# ok, now drop sand
sand_count <- 1
while(TRUE) {
  # new bit of sand
  sand = list(x=sandX, y=sandY)
  # drop it
  landed <- FALSE
  sand_count <- sand_count+1
  if (sand_count %% 1000 == 0)
    cat("count at", sand_count, "\n")
  while(sand$x >= 1 && sand$x <= nrow(map) &&
        sand$y < ncol(map)) {
    # check if we can drop down
#    cat("sand at", sand$x, ",", sand$y, "\n")
    if (map[sand$x, sand$y+1] == 0) {
      sand$y = sand$y + 1 # drop down
    } else if (sand$x <= 1 || map[sand$x-1, sand$y+1] == 0) {
      sand$y = sand$y + 1 # drop diagonal left
      sand$x = sand$x - 1
    } else if (sand$x >= nrow(map) || map[sand$x+1, sand$y+1] == 0) {
      sand$y = sand$y + 1
      sand$x = sand$x + 1
    } else { # can't drop so must be at stand still
      map[sand$x, sand$y] <- 2
      landed=TRUE
      break;
    }
  }
  if (!landed || sand$y == 1)
    break
}
image(map[,ncol(map):1])
sum(map == 2)

# part 2:

# we add in an "infinite" floor. Ofc the floor need only be from sandX +/- max(y)+2

# this is gonna consume a LOT more sand, but it's still probably doable?
fill_this |> summarise(max(y)) # maximum y is 169. So our floor is at 171

p2_fill <- fill_this |> bind_rows(data.frame(x = seq(500-171, 500+171), y=171))

# ok, make a map grid
minX = min(p2_fill$x, 500) - 1
minY = min(p2_fill$y, 0) - 1

sandX = 500 - minX
sandY = 0 - minY

p2_pos <- p2_fill |>
  mutate(x = x - minX, y = y - minY) |>
  select(x, y) |>
  as.matrix()

map <- matrix(0, nrow=max(p2_pos[,1]), ncol=max(p2_pos[,2]))
map[p2_pos] <- 1

image(map[,ncol(map):1])
