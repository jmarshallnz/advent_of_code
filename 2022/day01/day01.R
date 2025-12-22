library(tidyverse)

input <- readLines('2022/day01/input.txt')

elves <- tibble(elf = cumsum(input == ""), calories = input) |>
  filter(calories != "") |>
  mutate(calories = as.numeric(calories))

elves |>
  group_by(elf) |>
  summarise(total=sum(calories)) |>
  slice_max(total)

# part 2 is equally gentle!
elves |>
  group_by(elf) |>
  summarise(total=sum(calories)) |>
  slice_max(total, n=3) |>
  summarise(sum(total))
