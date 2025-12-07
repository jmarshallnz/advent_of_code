library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

raw <- readLines("2025/day06/input.txt") |>
  str_split(" +") |>
  map(\(x) x[x != ""])

ops  <- raw[[length(raw)]]
nums <- raw[-length(raw)] |>
  map(as.numeric) |>
  list_transpose()

`+`=`sum`
`*`=`prod`
map2_dbl(ops, nums, \(x, y) { do.call(what=x, as.list(y))}) |>
  sum()

# part 2 now the spaces are important
raw <- readLines("2025/day06/input.txt") |>
  str_split_fixed("", n=Inf)

gaps <- apply(raw, 2, \(x) all(x == " "))

data.frame(sum = cumsum(gaps), gap = gaps, t(raw)) |>
  rename(op = X5) |>
  mutate(op = na_if(op, " ")) |>
  fill(op) |>
  filter(!gap) |>
  mutate(number = as.numeric(paste(X1, X2, X3, X4, sep='')), .keep='unused') |>
  nest(number = number) |>
  mutate(value = map2_dbl(op, number, \(x, y) do.call(what=x, y))) |>
  summarise(sum(value))

