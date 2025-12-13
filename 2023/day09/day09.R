library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines("2023/day09/input.txt") |>
  str_split(" ") |>
  map(as.numeric)

# I don't think the repeated application of diff will be easy in a dataframe/
# tidyverse style setup, but probably fits accumulate?

# yup:
predict_next <- function(input) {
  diffs <- accumulate(seq_along(input), \(x, y) { ans=diff(x); if (all(ans == 0)) done(ans) else ans }, .init=input)
  reduce(rev(diffs)[-1], \(x, y) { last(x) + last(y) }, .init=0)
}

pred <- map_dbl(input, predict_next)
sum(pred)

# part 2 is the same but we need to go backwards instead
predict_prev <- function(input) {
  diffs <- accumulate(seq_along(input), \(x, y) { ans=diff(x); if (all(ans == 0)) done(ans) else ans }, .init=input)
  reduce(rev(diffs)[-1], \(x, y) { first(y) - first(x) }, .init=0)
}
pred <- map_dbl(input, predict_prev)
sum(pred)

predict_prev(input[[1]])
