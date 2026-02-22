library(tidyverse)

input <- read.table('2020/day01/input.txt', header=FALSE)

input |> cross_join(input) |>
  filter(V1.x != V1.y) |>
  mutate(sums = V1.x + V1.y,
         prod = V1.x * V1.y) |>
  filter(sums == 2020)

# part 2:

input |> cross_join(input) |>
  filter(V1.x != V1.y) |>
  cross_join(input |> rename(V1.z = V1)) |>
  filter(V1.x != V1.z, V1.y != V1.z) |>
  mutate(sums = V1.x + V1.y + V1.z,
         prod = V1.x * V1.y * V1.z) |>
  filter(sums == 2020)
