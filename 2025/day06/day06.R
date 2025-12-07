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
  nest(data=X1:X4) |>
  mutate(num = map(data, \(x) as.numeric(apply(x, 1, FUN=paste, collapse='')))) |>
  mutate(value = map2_dbl(op, num, \(x, y) do.call(what=x, as.list(y)))) |>
  summarise(sum(value))

