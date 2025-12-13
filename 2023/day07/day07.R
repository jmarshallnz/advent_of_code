library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

ranks <- tibble(card=c('A', 'K', 'Q', 'J', 'T', 9, 8, 7, 6, 5, 4, 3, 2)) |>
  mutate(rank = n()-row_number()+1)

input <- read.table("2023/day07/input.txt") |>
  mutate(hand = str_split(V1, '', n=5)) |>
  unnest(hand) |>
  left_join(ranks, join_by(hand == card)) |>
  group_by(V1, V2) |>
  mutate(index = n() - 1:n()) |>
  add_count(hand) |>
  summarise(card_rank = sum(rank*(16^index)),
            hand_rank = sum(n)) |>
  ungroup()

input |> arrange(hand_rank, card_rank) |>
  mutate(rank = row_number()) |>
  summarise(sum(V2*rank))

# part 2: Hmm, how does adding the J alter things...

#Now when we add the counts, we'll get both the J count AND the others.
# The J count then increases stuff I think?
# i.e. you could have:

# J<4 of a kind> 4*4
# J<3 of a kind> 3*3+1*1
# J<2 pair> 2*2+2*2
# J<1 pair> 2*2+1*2
# J<singles> 4*1
# JJ<3 of a kind> 3*3
# JJ<1 pair> 2*2+1*1
# JJ<singles> 3*1
# JJJ<pair> 2*2
# JJJ<singles> 2*1
# JJJJ<single> 1
# JJJJJ

# count the number that aren't J.
# count the number that ARE j.
# _change_ the J's to the number with the highest count

ranks_p2 <- tibble(card=c('A', 'K', 'Q', 'T', 9, 8, 7, 6, 5, 4, 3, 2, 'J')) |>
  mutate(rank = n()-row_number()+1)

hands <- read.table("2023/day07/input.txt") |>
  mutate(hand = str_split(V1, '', n=5)) |>
  unnest(hand) |>
  left_join(ranks_p2, join_by(hand == card)) |>
  group_by(V1, V2) |>
  mutate(index = n() - 1:n())

# find which thing is the biggest that we need to swap with a J
biggest <- hands |>
  group_by(V1) |> filter(hand != "J") |>
  add_count(hand) |>
  slice_max(n, with_ties = FALSE) |>
  select(V1, replace=hand)

# swap the J and we're done:
hands |>
  left_join(biggest) |>
  mutate(hand = if_else(hand == "J", replace, hand)) |>
  add_count(hand) |>
  summarise(card_rank = sum(rank*(16^index)),
            hand_rank = sum(n)) |>
  ungroup() |>
  arrange(hand_rank, card_rank) |>
  mutate(rank = row_number()) |>
  summarise(sum(V2*rank))
