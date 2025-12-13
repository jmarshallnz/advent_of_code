library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines("2023/day13/input.txt")
gaps <- input == ""

maps <- tibble(map = cumsum(gaps)+1, input=input) |>
  filter(input != "") |>
  group_by(map) |>
  summarise(in_split = list(str_split_fixed(input, '', n=Inf))) |>
  pull(in_split)

flip_x <- function(m, x) {
  if (x > ncol(m)/2)
    return(flip_x(m[,ncol(m):1], ncol(m)-x))
  all(m[,1:x] == m[,x:1+x])
}

flip_y <- function(m, x) {
  flip_x(t(m), x)
}

flip_x(maps[[1]], 3)
flip_y(maps[[1]], 16)

score_flip <- function(m) {
  x <- map_lgl(seq_len(ncol(m)-1), \(x) flip_x(m, x))
  y <- map_lgl(seq_len(nrow(m)-1), \(y) flip_y(m, y))
#  sx <- sum(which(x))
#  sy <- sum(which(y))
  return(list(x=which(x), y=which(y)*100))
}

score_flip(maps[[1]])

flip_scores <- map(maps, score_flip)

flip_scores |>
  summarise(score = sum(x) + sum(y))

# Part 2: there's a smudge. We have to repeat the above
#         first swapping one of the elements

# The maps aren't big, so this should be as easy as iterating,
# add a smudge, re-score until we get a different (non-zero!)
# flip_score and we're done

smudge_flip <- function(m) {
  old_score <- score_flip(m)
  for (i in 1:nrow(m)) {
    for (j in 1:ncol(m)) {
      # smudge and score
      sm <- m
      sm[i,j] <- if_else(m[i,j] == '.', '#', '.')
      score <- score_flip(sm)
      score$x <- setdiff(score$x, old_score$x)
      score$y <- setdiff(score$y, old_score$y)
      if (length(score$x) > 0)
        return(score$x)
      if (length(score$y) > 0)
        return(score$y)
      # take the new score if there's a non-zero entry that is
      # different to what we had before
    }
  }
  return(0) # panic!
}

ans <- map_dbl(maps, smudge_flip)

ans |>
  sum()
