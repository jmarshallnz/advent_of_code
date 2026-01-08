library(tidyverse)

program <- read.table(text="addx 15
addx -11
addx 6
addx -3
addx 5
addx -1
addx -8
addx 13
addx 4
noop
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx 5
addx -1
addx -35
addx 1
addx 24
addx -19
addx 1
addx 16
addx -11
noop
noop
addx 21
addx -15
noop
noop
addx -3
addx 9
addx 1
addx -3
addx 8
addx 1
addx 5
noop
noop
noop
noop
noop
addx -36
noop
addx 1
addx 7
noop
noop
noop
addx 2
addx 6
noop
noop
noop
noop
noop
addx 1
noop
noop
addx 7
addx 1
noop
addx -13
addx 13
addx 7
noop
addx 1
addx -33
noop
noop
noop
addx 2
noop
noop
noop
addx 8
noop
addx -1
addx 2
addx 1
noop
addx 17
addx -9
addx 1
addx 1
addx -3
addx 11
noop
noop
addx 1
noop
addx 1
noop
noop
addx -13
addx -19
addx 1
addx 3
addx 26
addx -30
addx 12
addx -1
addx 3
addx 1
noop
noop
noop
addx -9
addx 18
addx 1
addx 2
noop
noop
addx 9
noop
noop
noop
addx -1
addx 2
addx -37
addx 1
addx 3
noop
addx 15
addx -21
addx 22
addx -6
addx 1
noop
addx 2
addx 1
noop
addx -10
noop
noop
addx 20
addx 1
addx 2
addx 2
addx -6
addx -11
noop
noop
noop", fill=TRUE) |>
  rename(op = V1, arg = V2)

program <- read.table('2022/day10/input.txt', fill=TRUE) |>
  rename(op = V1, arg = V2)

cycles <- tibble(op = c('addx', 'noop'), cycles=c(2,1))

output <- program |>
  left_join(cycles) |>
  mutate(cycle = cumsum(cycles)) |>
  group_by(op) |>
  mutate(reg = cumsum(arg) + 1) |>
  ungroup() |>
  fill(reg)

# note our output is AFTER the 'cycle', not during it.
# during it will be the value before, so let's add another one.

filled_output <- output |> complete(cycle = full_seq(cycle,1)) |>
  fill(reg, .direction='down') |>
  mutate(cycle = cycle+1) |>
  bind_rows(tibble(cycle = 1:2, reg = 1)) |>
  arrange(cycle)

# part 1 is now a simple filter/summarise:
filled_output |>
  filter(cycle %in% seq(20, 220, by=40)) |>
  summarise(sum(cycle*reg))

# part 2: we now have our CRT as well to deal with.
# we have a # on the CRT if *during* cycle x the reg is in x-1,x+1
filled_output |>
  mutate(sprite_left = reg-1, sprite_right = reg+1,
         crt = ((cycle-1) %% 40),
         lin = ((cycle-1) %/% 40),
         ch = crt >= sprite_left & crt <= sprite_right) |>
  ggplot() +
  aes(x=crt, y=-lin, fill=ch) +
  geom_tile() +
  coord_fixed()
