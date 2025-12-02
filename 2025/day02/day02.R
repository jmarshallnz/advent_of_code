library(tidyverse)

# part 1:
read.table("2025/day02/input.txt", sep=",") |>
  pivot_longer(everything(), names_to="range", values_to="minmax") |>
  extract(minmax, into=c('from', 'to'), regex="([0-9]+)\\-([0-9]+)", convert=TRUE) |>
  mutate(digits_from=ceiling(log10(from+0.1)), digits_to = ceiling(log10(to+0.1))) |>
  mutate(from_fix=if_else((digits_from %% 2) != 0, 10^digits_from, from),
         to_fix=if_else((digits_to %% 2) != 0, 10^(digits_to-1)-1, to)) |>
  filter(from_fix <= to_fix) |>
  mutate(digits_from=ceiling(log10(from_fix+0.1)), digits_to = ceiling(log10(to_fix+0.1))) |>
  mutate(from_pat = from_fix %/% 10^(digits_from/2),
         to_pat =   to_fix %/% 10^(digits_to/2)) |>
  mutate(range = map2(from_pat, to_pat, seq)) |>
  unnest(range) |>
  mutate(range = as.numeric(paste(range, range, sep=''))) |>
  filter(range >= from & range <= to) |>
  summarise(sum(range))

# part 2: same idea, just we need to do it for 2, 3, 4, ... 10
# only trick bit is getting the right power out of it.
# possibly 'easier' to operate on strings?
find_invalids <- function(dat, rep_times=2) {
  dat |>
    mutate(from_fix=if_else((digits_from %% rep_times) != 0, 10^digits_from, from),
           to_fix=if_else((digits_to %% rep_times) != 0, 10^(digits_to-1)-1, to)) |>
    filter(from_fix <= to_fix) |>
    mutate(digits_from=ceiling(log10(from_fix+0.1)), digits_to = ceiling(log10(to_fix+0.1))) |>
    mutate(from_pat = from_fix %/% 10^(digits_from*(1 - 1/rep_times)),
           to_pat =   to_fix %/% 10^(digits_to*(1 - 1/rep_times))) |>
    mutate(range = map2(from_pat, to_pat, seq)) |>
    unnest(range) |>
    mutate(range = map_dbl(range, \(x) as.numeric(paste(rep(x, rep_times), collapse='')))) |>
    filter(range >= from & range <= to)
}

dat |> find_invalids(rep_times=3) |> print(n=100)
dat |> find_invalids(rep_times=4) |> print(n=100)

map_dfr(2:10, \(x) { dat |> find_invalids(x) }) |>
  pull(range) |> unique() |>
  sum()

# alternative: just generate all possible invalids and rule them out
invalids <- tibble(pattern_reps = 2:10) |>
  mutate(min_pattern = 1,
         max_pattern = 10^floor(10/pattern_reps)-1) |>
  mutate(pattern = map2(min_pattern, max_pattern, seq)) |>
  unnest(pattern) |>
  mutate(pattern = map2(pattern, pattern_reps, \(x, y) paste(rep(x, y), collapse=''))) |>
  mutate(num_pattern = map_dbl(pattern, as.numeric)) |>
  select(num_pattern) |>
  unique()

# ok, join our invalids to our ranges and rule them out
ranges <- read.table("2025/day02/input.txt", sep=",") |>
  pivot_longer(everything(), names_to="range", values_to="minmax") |>
  extract(minmax, into=c('from', 'to'), regex="([0-9]+)\\-([0-9]+)", convert=TRUE)

ranges |> cross_join(invalids) |>
  filter(num_pattern >= from, num_pattern <= to) |>
  summarise(sum(num_pattern))

# could do the join with a join window:
ranges |> left_join(invalids, by = join_by(from <= num_pattern, to >= num_pattern)) |>
  summarise(sum(num_pattern, na.rm=TRUE))
