library(tidyverse)

moves <- read.table("2022/day09/input.txt", header=FALSE) |>
  rename(dir=V1, len=V2) |>
  rowwise() |>
  group_split()

moves <- read.table(text='R 5
U 8
L 8
D 3
R 17
D 10
L 25
U 20')|>
  rename(dir=V1, len=V2) |>
  rowwise() |>
  group_split()

# original position is at 0,0

head_pos <- c(0,0)
tail_pos <- c(0,0)
visited <- list(tail_pos)

head_moves <- list(U = c(0,-1), D = c(0, 1), L = c(-1, 0), R = c(1, 0))

# do the movement of head and tail
for (i in 1:length(moves)) {
  move <- moves[[i]]
  delta <- head_moves[[move$dir]]
  for (j in 1:move$len) {
    head_pos <- head_pos + delta
    # move tail. Always moves towards head. If in the same row moves 1, else
    # moves diagonally.
    dtail <- head_pos - tail_pos
    if (any(abs(dtail) > 1)) { # need to move the tail once in the same direction as the head moved
      # if we're on-axis we move once, else we move diagonally
      tail_pos <- tail_pos + sign(dtail)
      # add to our visited
      visited[[length(visited) + 1]] <- tail_pos
    }
  }
}

do.call(rbind, visited) |> unique() |> nrow()

# part 2 is the same, except now we need to move multiple tails the same way as before.
# our head_pos is now still the same, but we have 10 of them.

head_pos <- c(0,0)
tail_pos <- rep(list(c(0,0)), 9)
visited <- list(tail_pos[[9]]) # only track the 10th snake piece

head_moves <- list(U = c(0,-1), D = c(0, 1), L = c(-1, 0), R = c(1, 0))

# do the movement of head and tail
for (i in 1:length(moves)) {
  move <- moves[[i]]
  delta <- head_moves[[move$dir]]
  for (j in 1:move$len) {
    head_pos <- head_pos + delta
    # move tail. Always moves towards head. If in the same row moves 1, else
    # moves diagonally.
    prev_pos <- head_pos
    for (k in 1:length(tail_pos)) {
      # move tail. Always moves towards head. If in the same row moves 1, else
      # moves diagonally.
      dtail <- prev_pos - tail_pos[[k]]
      if (any(abs(dtail) > 1)) { # need to move the tail once in the same direction as the head moved
        # if we're on-axis we move once, else we move diagonally
        tail_pos[[k]] <- tail_pos[[k]] + sign(dtail)
      }
      prev_pos <- tail_pos[[k]]
    }
    # add to our visited
    visited[[length(visited) + 1]] <- tail_pos[[9]]
  }
}

do.call(rbind, visited) |> as.data.frame() |>
  ggplot() +
  aes(x=V1, y=V2) +
  geom_tile()

do.call(rbind, visited) |> unique() |> nrow()
