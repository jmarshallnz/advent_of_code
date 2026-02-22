library(tidyverse)

input <- read.table("2021/day02/input.txt", sep=" ", header=FALSE) |>
  setNames(c('dir', 'amount'))

# order doesn't matter, so just count away
amounts <- input |> group_by(dir) |>
  summarise(amount = sum(amount)) |>
  deframe()

amounts['forward'] * (amounts['down'] - amounts['up'])

# part 2: order now matters, as movements forward changes depth based on another measure
input |>
  mutate(aim = cumsum(amount*ifelse(dir == 'down', 1, ifelse(dir == 'up', -1, 0)))) |>
  mutate(position = cumsum(amount*ifelse(dir == 'forward', 1, 0))) |>
  mutate(depth = cumsum(aim*amount*ifelse(dir == 'forward', 1, 0))) |>
  slice(n()) |>
  summarise(position*depth)
