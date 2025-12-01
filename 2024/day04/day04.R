library(tidyverse)

d <- read_lines('day4/input.txt') |>
  str_split_fixed(pattern='', n=Inf)

# OK, rows and columns are easy
rows <- d |> apply(1, paste0, collapse='')
cols <- d |> apply(2, paste0, collapse='')

# Diagonals a bit trickier. This is a little messy. Could probably flip left to right instead?
diags <- expand_grid(diag=-(nrow(d)-1):(ncol(d)-1), flip=c(FALSE, TRUE), row=1:nrow(d), col=1:ncol(d)) |>
  filter(row == col+diag & !flip |
        (nrow(d)-row+1) == col+diag & flip)

dias <- diags |>
  group_by(diag, flip) |>
  summarise(entries = paste0(d[cbind(row, col)], collapse='')) |>
  pull(entries)

all <- c(rows, cols, dias)

(str_count(all, "XMAS") +
 str_count(all, "SAMX")) |>
  sum()

# part 2.
# all we need to do is extract all 3x3 submatrices and then
# check them (A must be center, M/S in opposite diagonals)
# this is actually easier than the previous
check_x_mas <- function(mat) {
  mat[2,2] == "A" &&
  all(sort(c(mat[1,1], mat[3,3])) == c("M", "S")) &&
  all(sort(c(mat[3,1], mat[1,3])) == c("M", "S"))
}

expand_grid(row=1:(nrow(d)-2), col=1:(nrow(d)-2)) |>
  mutate(submatrix = map2(row, col, ~d[.x:(.x+2), .y:(.y+2)])) |>
  mutate(found = map_lgl(submatrix, check_x_mas)) |>
  summarise(sum(found))

