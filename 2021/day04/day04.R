library(tidyverse)

input <- readLines("2021/day04/input.txt")

numbers <- input[1] |> str_split(",") |> unlist() |> as.integer()

grids <- tibble(input=input[-1], gap=cumsum(input == "")) |>
  filter(input != "") |>
  mutate(input = str_trim(input)) |>
  group_by(gap) |>
  summarise(grid = str_split_fixed(input, "[ ]+", n=5) |> as.data.frame() |> apply(1, as.numeric) |> list()) |>
  pull(grid)

# perfect, now just check-off the numbers as we go

boards <- map(grids, \(x) x == -1) # start

for (num in numbers) {
  boards <- map2(boards, grids, \(x, y) x | (y == num))
  # see if we've won
  won <- map_lgl(boards, \(x) any(c(rowSums(x), colSums(x)) == 5))
  if (sum(won) == 1)
    break
}

board <- boards[[which(won)]]
num
grid <- grids[[which(won)]]
sum(grid[!board]*num)

# part 2:
boards <- map(grids, \(x) x == -1) # start
prev_won <- map_lgl(boards, \(x) any(c(rowSums(x), colSums(x)) == 5))
for (num in numbers) {
  boards <- map2(boards, grids, \(x, y) x | (y == num))
  # see if we've won
  won <- map_lgl(boards, \(x) any(c(rowSums(x), colSums(x)) == 5))
  if (sum(won) == 100)
    break
  prev_won = won
}

board <- boards[[which(!prev_won)]]
num
grid <- grids[[which(!prev_won)]]
sum(grid[!board]*num)
