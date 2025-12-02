library(tidyverse)

# Part 1: Strategy is just wrap around with a cumsum() and count the zeros
read.table("2025/day01/input.txt", header=FALSE) |>
  extract(V1, into=c("dir", "amount"), regex="([LR])([0-9]+)", convert = TRUE) |>
  mutate(amount = amount*if_else(dir == "L", -1, 1)) |>
  mutate(next_pos = (50 + cumsum(amount)) %% 100) |>
  summarise(answer = sum(next_pos == 0))

# Part 2: we need to count when we go past zero as well.
# We can have full wraps in one go (e.g. R1000) so first step is
# counting those. Then just check when we move past 0 or 100
# in the usual addition from previous, noting that if we're already
# at 0 then we don't count moving from it
read.table("2025/day01/input.txt", header=FALSE) |>
  extract(V1, into=c("dir", "amount"), regex="([LR])([0-9]+)", convert = TRUE) |>
  mutate(wrap = amount %/% 100, amount = amount %% 100) |>
  mutate(amount = amount*if_else(dir == "L", -1, 1)) |>
  mutate(next_pos = (50 + cumsum(amount)) %% 100,
         curr_pos = lag(next_pos, default=50)) |>
  mutate(past_zero = case_when(curr_pos == 0 ~ 0, # moving away from zero
                               amount < 0 & curr_pos + amount <= 0 ~ 1,   # wrapped or hit zero,
                               amount > 0 & curr_pos + amount >= 100 ~ 1, # wrapped or hit zero
                               TRUE ~ 0) # LOL on true -> false
         ) |>
  mutate(total_zeros = wrap + past_zero) |>
  mutate(answer = sum(total_zeros))
