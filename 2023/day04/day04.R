library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines("2023/day04/input.txt") |>
  str_split_fixed("[|:]", n=3) |>
  as.data.frame()

winning_cards <- input |> mutate(winners = str_split(V2, " "),
                cards = str_split(V3, " "), .keep='unused') |>
  mutate(across(c(winners, cards), \(x) map(x, \(y) { y = as.numeric(y); y[!is.na(y)] }))) |>
  unnest(winners) |>
  unnest(cards) |>
  group_by(V1) |>
  summarise(n = sum(winners == cards))

winning_cards |>
  filter(n != 0) |>
  mutate(power = 2^(n-1)) |>
  summarise(sum(power))

# part 2
won_copies <- winning_cards |>
  pull(n)

cards <- rep(1, length(won_copies))
for (i in seq_along(won_copies)) {
  if (won_copies[i] > 0)
    cards[i+1:won_copies[i]] <- cards[i+1:won_copies[i]]+cards[i]
}
