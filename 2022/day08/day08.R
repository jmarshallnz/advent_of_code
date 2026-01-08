library(tidyverse)

map <- readLines('2022/day08/input.txt') |>
  str_split('') |>
  map(as.numeric) |>
  do.call(rbind, args=_)

map
left_less_than <- function(x) {
  y <- logical(length(x))
  for (i in 1:length(x)) {
    y[i] <- all(x[seq_len(i-1)] < x[i])
  }
  y
}
vl <- t(apply(map, 1, left_less_than))
vr <- t(apply(map, 1, \(x) rev(left_less_than(rev(x)))))
vt <- apply(map, 2, left_less_than)
vb <- apply(map, 2,  \(x) rev(left_less_than(rev(x))))

sum(vl | vr | vt | vb)

# part 2 we want the distance from each tree to the tallest one from them
# to the left
view_to_left <- function(x) {
  y <- integer(length(x))
  for (i in 1:length(x)) {
    left_higher <- which(x[seq_len(i-1)] >= x[i])
    y[i] <- if (length(left_higher) > 0) i-max(left_higher) else i-1
  }
  y
}
dl <- t(apply(map, 1, view_to_left))
dr <- t(apply(map, 1, \(x) rev(view_to_left(rev(x)))))
dt <- apply(map, 2, view_to_left)
db <- apply(map, 2,  \(x) rev(view_to_left(rev(x))))
max(dl*dr*dt*db)
