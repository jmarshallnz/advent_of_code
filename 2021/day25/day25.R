library(tidyverse)

map <- readLines('2021/day25/input.txt') |>
  str_split_fixed('', n=Inf)

# move the > first then the v
east <- map == '>'
down <- map == 'v'

next_east_col <- c(2:ncol(east),1)
next_down_row <- c(2:nrow(down),1)

# check east moves
for (i in 1:100000) {
  cat("moving...", i, "\n")
east_can_move <- east & !east[,next_east_col] & !down[,next_east_col]
# these ones can move:
can_move <- which(east_can_move, arr.ind=TRUE)
# they move RIGHT
east[can_move] <- FALSE; can_move[,2] = (can_move[,2] %% ncol(east)) + 1
east[can_move] <- TRUE

# check down moves
down_can_move <- down & !east[next_down_row,] & !down[next_down_row,]
# these ones can move:
can_move <- which(down_can_move, arr.ind=TRUE)
# they move RIGHT
down[can_move] <- FALSE; can_move[,1] = (can_move[,1] %% nrow(down)) + 1
down[can_move] <- TRUE

if (sum(east_can_move) == 0 &&
    sum(down_can_move) == 0)
  break
}


