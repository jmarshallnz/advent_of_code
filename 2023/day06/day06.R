library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- read.table("2023/day06/input.txt", row.names = 1) |>
  t() |> as.data.frame()

input |>
  mutate(hold_times = map(`Time:`, \(x) seq(0, x))) |>
  unnest(hold_times) |>
  mutate(speed = hold_times,
         travel_time = `Time:`-hold_times,
         distance = speed*travel_time) |>
  group_by(`Time:`, `Distance:`) |>
  summarise(times_bet = sum(distance > `Distance:`)) |>
  ungroup() |>
  summarise(prod(times_bet))

# part 2: clearly we can't iterate, but it's a pretty straight forward equation:
compressed <- input |>
  summarise(across(everything(), \(x) as.numeric(paste0(x, collapse=''))))

time <- compressed$`Time:`
dist <- compressed$`Distance:`

# Given hold_time we have the formula for distance = hold_time*(time - hold_time)
# so we need to know when hold_time*time - hold_time^2 > dist or when h^2 - ht + dist < 0
# so just need to find the roots of this, which is easy with the quadratic formula:
roots <- c(ceiling((time - sqrt(time^2 - 4*dist)) / 2),
           floor((time + sqrt(time^2 - 4*dist))/2))

diff(roots)+1

