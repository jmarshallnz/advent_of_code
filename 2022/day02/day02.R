library(tidyverse)

input <- read.table("2022/day02/input.txt")

win_lose <- tribble(~elf, ~me, ~play, ~win,
                    'A', 'X', 1, 0, # rock, rock
                    'A', 'Y', 2, 1,# rock, paper
                    'A', 'Z', 3, -1,# rock, scissors
                    'B', 'X', 1, -1, # paper, rock
                    'B', 'Y', 2, 0,
                    'B', 'Z', 3, 1,
                    'C', 'X', 1, 1,# scissors, rock
                    'C', 'Y', 2, -1,
                    'C', 'Z', 3, 0)

win_lose

# part 1: play directly
input |> left_join(win_lose, by=join_by(V1 == elf, V2 == me)) |>
  mutate(score = win*3+3 + play) |>
  summarise(total = sum(score))

# part 2: X is lose, Y is draw, Z is win, so convert to our -1,0,1 win then join
#         that instead
input |> mutate(win = as.integer(as.factor(V2)) - 2) |>
  left_join(win_lose, by=join_by(V1 == elf, win == win)) |>
  mutate(score = win*3+3 + play) |>
  summarise(total = sum(score))
