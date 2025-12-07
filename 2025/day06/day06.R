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
  rename(op = last_col()) |>        # picks X5 without using the 5 so it'll work with more general input
  mutate(op = na_if(op, " ")) |>
  fill(op) |>
  filter(!gap) |>
  tibble::rowid_to_column() |>      # paste the X1..X4 columns together. Coded via pivot_longer so it'll work with more
  pivot_longer(starts_with("X")) |>
  group_by(sum,op,rowid) |>
  summarise(value = as.numeric(paste(value, collapse=''))) |>
  group_by(sum) |>
  summarise(final = do.call(first(op), as.list(value))) |>
  summarise(sum(final))

