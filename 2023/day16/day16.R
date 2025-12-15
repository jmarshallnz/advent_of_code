library(tidyverse)
library(igraph)
options(digits = 22,
        pillar.max_dec_width = 22)

map <-
".|...\\....
|.-.\\.....
.....|-...
........|.
..........
.........\\
..../.\\\\..
.-.-/..|..
.|....-|.\\
..//.|...." |> str_split("\n") |> unlist() |>
  str_split_fixed('', n=Inf) |>
  as.data.frame()

map <- readLines("2023/day16/input.txt") |>
  str_split_fixed('', n=Inf) |>
  as.data.frame()

# OK, we need to generate the appropriate thing
# we need to know the position of (multiple beams)
# and their direction. Then use position to
# identify the next obstacle and then direction
# to decide what to do.
moves <- tribble(~letter, ~direction, ~nd,
                 '\\', 'L', 'U',
                 '\\', 'R', 'D',
                 '\\', 'U', 'L',
                 '\\', 'D', 'R',
                 '/', 'L', 'D',
                 '/', 'R', 'U',
                 '/', 'U', 'R',
                 '/', 'D', 'L',
                 '-', 'R', 'R',
                 '-', 'L', 'L',
                 '-', 'U', 'LR',
                 '-', 'D', 'LR',
                 '|', 'U', 'U',
                 '|', 'D', 'D',
                 '|', 'L', 'UD',
                 '|', 'R', 'UD',
                 '.', 'L', 'L',
                 '.', 'R', 'R',
                 '.', 'U', 'U',
                 '.', 'D', 'D')

moves <- moves |> pivot_wider(names_from='direction', values_from='nd') |>
  tibble::column_to_rownames('letter') |>
  as.matrix()

dpos <- list('D' = c(1, 0), 'U' = c(-1, 0), 'L' = c(0, -1), 'R' = c(0, 1))
l1 <- left |> select(-l2) |> pivot_wider(names_from=direction, values_from=l1) |>
  tibble::column_to_rownames('letter') |>
  as.matrix()

l2 <- left |> select(-l1) |> pivot_wider(names_from=direction, values_from=l2) |>
  tibble::column_to_rownames('letter') |>
  as.matrix()
moves

do_beam <- function(pos, dir) {

  cat("doing beam from ", pos, " direction ", dir, "\n")
  add_to_beam_history <- function(beam) {
    # check OOB
    if (beam$pos[1] > nrow(map) ||
        beam$pos[2] > ncol(map) ||
        beam$pos[1] < 1 ||
        beam$pos[2] < 1)
      return(FALSE) # this beam is done...

    # check if it's already there
 #   cat('checking at x=', beam$pos[1], 'y=', beam$pos[2], 'dir=', beam$dir, "\n")
    if (movemap[beam$pos[1], beam$pos[2], beam$dir])
      return(FALSE) # already there
    movemap[beam$pos[1], beam$pos[2], beam$dir] <<- TRUE
    return(TRUE)
  }

  # OK, now we iterate, increasing our beam length as we go until
  # they all leave the arena?
  beams <- list(list(pos = pos, dir = dir))
  movemap <- array(FALSE, dim=c(nrow(map), ncol(map), 4),
                   dimnames = list(NULL, NULL, c("L", "R", "U", "D")))

  steps <- 1
  while (length(beams) > 0) {
    beam <- beams[[1]]
    while (TRUE) {
      # check if we've been here before in this direction
      if (!add_to_beam_history(beam)) {
   #     cat("seen beam before\n")
        break # we're done with this beam
      }
      if ((steps %% 10000) == 0)
        cat("step: ", steps, "has", length(beams), "beams\n")
  #    if (steps > 30)
  #      break
      # make a move
      at <- map[beam$pos]
 #     cat("at position:", beam$pos, "which is:", at, "going", beam$dir, "\n")
      new_dir <- moves[at, beam$dir]
      if (nchar(new_dir) > 1) {
        # splitter - create a new beam
        new_beam_dir <- substr(new_dir, 2, 2)
        new_beam_pos <- beam$pos + dpos[[new_beam_dir]]
   #     cat('new beam at', new_beam_pos, '\n')
        beams[[length(beams)+1]] <- list(pos = new_beam_pos, dir = new_beam_dir)
        # this beam can take the first path
        new_dir <- substr(new_dir, 1, 1)
      }
      beam$dir <- new_dir
      beam$pos <- beam$pos + dpos[[new_dir]]
    }
    # remove this beam
    beams <- beams[-1]
  }

  apply(movemap, 1:2, any) |>
    sum()
}

starts <- data.frame(x = 1:nrow(map), y = 1, dir='R') |>
  bind_rows(data.frame(x = 1:nrow(map), y = ncol(map), dir = 'L')) |>
  bind_rows(data.frame(x = 1, y=1:ncol(map), dir = 'D')) |>
  bind_rows(data.frame(x=nrow(map), y=1:ncol(map), dir='U')) |> rowwise() |>
  group_split()

map_int(starts, \(start) do_beam(cbind(start$x, start$y), start$dir))
