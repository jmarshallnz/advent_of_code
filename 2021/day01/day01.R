library(tidyverse)

input <- read.table("2021/day01/input.txt", header=FALSE)

input |> mutate(increased = lag(V1) < V1) |>
  summarise(sum(increased, na.rm=TRUE))

# part 2: rolling sum
input |>
  mutate(rolling=V1 + lag(V1, n=1) + lag(V1, n=2)) |>
  mutate(increased = lag(rolling) < rolling) |>
  summarise(sum(increased, na.rm=TRUE))
