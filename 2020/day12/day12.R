library(tidyverse)

options(digits = 22,
        pillar.max_dec_width = 22)

input <- "F10
N3
F7
R90
F11" |> str_split("\n") |> unlist()
input <- readLines('2020/day12/input.txt')

instructions <- tibble(input = input) |> extract(input, into=c('command', 'value'), regex="([A-Z])([0-9]+)", convert=TRUE) |>
  rowwise() |> group_split() |>
  map(as.list)

# OK, now we need to know which way we're facing and we're we are (our state)

dx <- list(N = c(0, 1), E = c(1, 0), W = c(-1, 0), S = c(0, -1)) |>
  map(\(x) matrix(x, ncol=1))

rot_mat <- function(angle) {
  matrix(c(round(cos(angle/180*pi)), -round(sin(angle/180*pi)),
         round(sin(angle/180*pi)), round(cos(angle/180*pi))), nrow=2)
}

facing <- matrix(c(1,0), ncol=1) # facing East
pos <- matrix(c(0,0), ncol=1)
dirs <- list(lst(pos, facing))
for (i in instructions) {
  if (i$command %in% names(dx)) {
    # move
    pos <- pos + dx[[i$command]]*i$value
  } else if (i$command == 'L') {
    facing <- rot_mat(-i$value) %*% facing
  } else if (i$command == 'R') {
    facing <- rot_mat(i$value) %*% facing
  } else if (i$command == 'F') {
    # forward
    pos <- pos + facing*i$value
  } else {
    cat('uuhuj\n')
  }
  dirs[[length(dirs)+1]] <- lst(pos, facing)
}
sum(abs(pos))

# Part 2: NEWS move a waypoint. LR rotate the waypoint around the ship. F moves
# the ship AND the waypoint K steps towards the waypoint (i.e. k=2 moves the ship 2 times the waypoint
# distance away).

waypoint <- matrix(c(10, 1), ncol=1) # offset from the ship
ship <- matrix(c(0,0), ncol=1) # absolute coordinates of the ship

dirs <- list(lst(ship, waypoint))
for (i in instructions) {
  if (i$command %in% names(dx)) {
    # move the waypoint
    waypoint <- waypoint + dx[[i$command]]*i$value
  } else if (i$command == 'L') {
    # rotate the waypoint around the ship
    waypoint <- rot_mat(-i$value) %*% waypoint
  } else if (i$command == 'R') {
    waypoint <- rot_mat(i$value) %*% waypoint
  } else if (i$command == 'F') {
    # forward: Move the ship. The waypoint stays relative
    ship <- ship + waypoint*i$value
  } else {
    cat('uuhuj\n')
  }
  dirs[[length(dirs)+1]] <- lst(ship, waypoint)
}
sum(abs(ship))

