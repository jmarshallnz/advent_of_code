library(tidyverse)

input <- read.csv("2021/day13/input.txt", sep=',', header=FALSE)

dots <- input |> mutate(V1 = as.numeric(V1)) |>
  filter(!is.na(V1)) |> set_names(c('x', 'y'))

folds <- input |> filter(str_detect(V1, 'fold')) |>
  extract(V1, into=c('wch', 'where'), regex="([xy])=([0-9]+)", convert=TRUE) |>
  select(wch, where)

# ok, do the first fold
fold <- function(dots, wch, where) {
  if (wch == 'x') { # folding along x
    new_dots <- dots |>
      mutate(x = if_else(x < where, x, where-(x-where)))
  } else {
    new_dots <- dots |>
      mutate(y = if_else(y < where, y, where-(y-where)))
  }
  new_dots |> unique()
}

fold(dots, folds$wch[1], folds$where[1]) |>
  nrow()

done <- reduce2(folds$wch, folds$where, fold, .init=dots)

# ok, now plot
ggplot(done) + aes(x=x, y=-y) + geom_point()
