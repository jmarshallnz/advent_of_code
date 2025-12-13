library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines("2025/day10/input.txt")

# seems some sort of (integer) programming problem I think.

# suspect the joltages (which seem to be one per light?) are some sort
# of "actually, you need to press the button at least that many times"
# thing.

lights <- tibble(light=sub("\\[([#\\.]+).*", "\\1", input)) |>
  mutate(on = map(light, \(x) ((str_split(x, '') |> unlist()) == "#") |> as.integer())) |>
  mutate(name = row_number()) |>
  select(name, on)

switches <- sub(".*\\](.*)\\{.*", "\\1", x=input) |>
  str_split(" ") |>
  enframe() |>
  unnest(value) |>
  filter(value != "") |>
  extract(value, into='range', regex="([0-9,]+)") |>
  mutate(range = map(range, \(x) str_split(x, pattern=',') |> unlist() |> as.numeric())) |>
  left_join(lights) |>
  mutate(len = lengths(on)) |>
  mutate(toggle = map2(len, range, \(x, y) { n = integer(x); n[y+1] <- 1L; n })) |>
  group_by(name) |>
  mutate(switch=row_number()) |>
  select(name, switch, toggle)

switches |>
  filter(name == 1) |>
  pull(on)

all_poss_switching <-
  lights |> left_join(switches) |>
  group_by(name, on) |>
  summarise(num = n()) |>
  mutate(poss = map(num, \(x) { sets::set_power(seq_len(x)) |> as.list() |> map(as.integer) } )) |>
  unnest(poss)

all_poss_switching |>
  mutate(poss = map(poss, as.integer)) |>
  mutate(n_pos = row_number()) |>
  unnest(poss) |>
  rename(switch = poss) |>
  left_join(switches) |>
  group_by(name, on, n_pos) |>
  summarise(switched=list(reduce(toggle, xor)),
            num_switched = n()) |>
  mutate(match = map2_lgl(on, switched, \(x, y) all(x == y))) |>
  filter(match) |>
  group_by(name) |>
  summarise(min_switched = min(num_switched)) |>
  summarise(sum(min_switched))

# OK, part 2 is then the same idea, but instead of toggling on and off
# we instead increment counters

# our counters are pretty high, so we can't do the same "try everything"
# out, and instead need to be smart.

# again it's an integer programming problem. Find the number of button
# presses that produce the output.

# so let's setup the integer program first. We'll need the matrix
# produced by the switches, which we already have done (just not in that
# form).

library(lpSolve)

X <- switches |>
  group_by(name) |>
  summarise(matrix = list(simplify2array(toggle)))

Y <- sub(".*\\{(.*)\\}.*", "\\1", input) |>
  str_split(',') |>
  map(as.numeric) |>
  map(\(x) matrix(x, ncol=1))

X |> left_join(Y |> enframe()) |>
  mutate(ans = map2_dbl(matrix, value, \(x, y) lp("min", rep(1, ncol(x)), x, '==', y, all.int=TRUE)$objval)) -> lp

# Just for fun try lm() instead:
test <- X |> left_join(Y |> enframe()) |>
  mutate(ans = map2_dbl(matrix, value, \(x, y) lm(y ~ -1 + x) |> coef() |> sum() |> round()))
