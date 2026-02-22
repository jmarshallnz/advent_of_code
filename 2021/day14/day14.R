library(tidyverse)

input = readLines("2021/day14/input.txt")

input = 'NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C' |> str_split("\n") |> unlist()

template <- input[1] |> str_split('') |> unlist()

rules <- input[-c(1:2)] |> str_split_fixed(" -> ", n=2) |>
  as.data.frame() |> set_names(c('pair', 'insert')) |>
  mutate(pair1 = paste0(str_sub(pair, 1, 1), insert),
         pair2 = paste0(insert, str_sub(pair, 2, 2))) |>
  select(-insert) |>
  pivot_longer(pair1:pair2, names_to ='wch', values_to='new_pair')

# just need to count how many of each pair we have.
pairs <- tibble(x = template) |> mutate(y = lead(template)) |>
  mutate(pair = paste(x, y, sep='')) |> filter(!is.na(y)) |>
  count(pair)

# ok, now each time we apply a rule we take our existing counts and sum over the new pairs
apply_rules <- function(pairs, rules) {
  rules |> left_join(pairs) |>
    group_by(pair=new_pair) |>
    summarise(n = sum(n, na.rm=TRUE))
}

out <- reduce(1:10, \(x, y) apply_rules(x, rules), .init=pairs)

# now count each value, noting that each one will occur twice, except the start
# and end letters, where we need to minus one, divide by two and add one (possibly
# to the same letter?)
# I guess if any are odd we could do that, but potentially it could be even if
# it's the same letter front and back, so best to treat it properly
out |> mutate(letter = str_split(pair, '')) |>
  unnest(letter) |>
  group_by(letter) |>
  summarise(n = sum(n, na.rm=TRUE)) |>
  mutate(n = if_else(letter == template[1], n-1, n)) |>
  mutate(n = if_else(letter == template[length(template)], n-1, n)) |>
  mutate(n = n/2) |>
  mutate(n = if_else(letter == template[1], n+1, n)) |>
  mutate(n = if_else(letter == template[length(template)], n+1, n)) |>
  summarise(ans = max(n) - min(n))

# part 2: run for 40 steps
out2 <- reduce(1:30, \(x, y) apply_rules(x, rules), .init=out)
out2 |> mutate(letter = str_split(pair, '')) |>
  unnest(letter) |>
  group_by(letter) |>
  summarise(n = sum(n, na.rm=TRUE)) |>
  mutate(n = if_else(letter == template[1], n-1, n)) |>
  mutate(n = if_else(letter == template[length(template)], n-1, n)) |>
  mutate(n = n/2) |>
  mutate(n = if_else(letter == template[1], n+1, n)) |>
  mutate(n = if_else(letter == template[length(template)], n+1, n)) |>
  summarise(ans = max(n) - min(n))
