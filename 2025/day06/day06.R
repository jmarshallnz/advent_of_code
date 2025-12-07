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

# paste down the columns and convert to numbers
nums <- apply(raw[-nrow(raw),], 2, \(x) as.numeric(paste(x, collapse='')))

# gather together and do the operation
data.frame(nums, op=raw[nrow(raw),]) |>
  mutate(sum = cumsum(is.na(nums))) |>
  filter(!is.na(nums)) |>
  group_by(sum) |>
  summarise(final = do.call(first(op), as.list(nums))) |>
  summarise(sum(final))

