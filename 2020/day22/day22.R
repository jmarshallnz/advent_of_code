library(tidyverse)
library(collections)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines('2020/day22/input.txt')
input <- 'Player 1:
9
2
6
3
1

Player 2:
5
8
4
7
10' |> str_split('\n') |> unlist()

input <- 'Player 1:
43
19

Player 2:
2
29
14' |> str_split('\n') |> unlist()

decks <- tibble(input, player = cumsum(str_detect(input, "Player"))) |>
  filter(!str_detect(input, "Player"), input != "") |>
  group_by(player) |>
  mutate(input = as.integer(input)) |>
  summarise(input = list(input)) |>
  deframe() |> map(as.list) |> map(queue)

play_round <- function(decks) {
  card1 <- decks[[1]]$pop()
  card2 <- decks[[2]]$pop()
  cat("card1 is", card1, "card2 is", card2, "sizes are", decks[[1]]$size(), decks[[2]]$size(), "\n")
  if (card1 > card2) {
    decks[[1]]$push(card1)$push(card2)
  } else {
    decks[[2]]$push(card2)$push(card1)
  }
  return(decks)
}

max_rounds <- 1000
curr <- decks
for (i in 1:max_rounds) {
  if (i %% 100 == 0) {
    cat("round", i, "\n")
  }
  curr <- play_round(curr)
  if (any(map_lgl(curr, \(x) x$size() == 0))) {
    break
  }
}

winner <- which(map_lgl(curr, \(x) x$size() != 0))
winner
winning_cards <- curr[[winner]]$as_list() |> unlist()

(winning_cards*rev(seq_along(winning_cards))) |>
  sum()

# Part 2: recursive combat

play_game <- function(queue_decks, level = 0) {
  # iterate across rounds
  prev_deck <- dict()# previous decks
#  prev_deck2 <- dict()
 # prev_decks <- list()
  decks <- list(queue(queue_decks[[1]]), queue(queue_decks[[2]]))
  rounds <- 0
  while (TRUE) {
    # pop a card off the decks
    rounds <- rounds + 1
    if (level <= 4 && rounds %% 10000 == 0) {
      cat("up to round", rounds, "of level", level, "with sizes", decks[[1]]$size(), decks[[2]]$size(), "\n")
      cat("cards are", decks[[1]]$as_list() |> as.integer(), "|", decks[[2]]$as_list() |> as.integer(), "\n")
    }
#    cat("playing round with sizes", decks[[1]]$size(), decks[[2]]$size(), "\n")
    card1 <- decks[[1]]$pop()
    card2 <- decks[[2]]$pop()
    player1_wins <- card1 >= card2
    if (card1 <= decks[[1]]$size() && card2 <= decks[[2]]$size()) {
      # recurse in to play a subgame with card1 and card2 cards respectively
      sub_decks <- list(decks[[1]]$as_list()[seq_len(card1)],
                        decks[[2]]$as_list()[seq_len(card2)])
#      cat("playing subgame with sizes", sub_decks[[1]]$size(), sub_decks[[2]]$size(), "\n")
      outcome <- play_game(sub_decks, level = level+1)
      player1_wins <- outcome$winner == 1
    }
    if (player1_wins) {
      # player 1 wins this round
#      cat("player 1 wins the round\n")
      decks[[1]]$push(card1)$push(card2)
      if (decks[[2]]$size() == 0) {
        return(list(winner=1, deck=decks[[1]]))
      }
    } else {
      # player 2 wins
#      cat("player 2 wins the round\n")
      decks[[2]]$push(card2)$push(card1)
      if (decks[[1]]$size() == 0) {
        # player 2 wins
        return(list(winner=2, deck=decks[[2]]))
      }
    }
    # noone has won yet. check if our decks is any different to a previous deck
    deck_vec <- c(decks[[1]]$as_list() |> as.integer(), 0, decks[[2]]$as_list() |> as.integer())
    match <- prev_deck$get(deck_vec, default=0)
    if (match > 0) {
      cat("hit a match in both decks at round", rounds, "repeats from", match, "\n")
      return(list(winner=1, deck=decks[[1]]))
    } else {
      prev_deck$set(deck_vec, rounds)
    }
#     for (i in seq_along(prev_decks)) {
#       d <- prev_decks[[i]]
#       if (decks[[1]]$size() == length(d[[1]])) {
#         match <- TRUE
#         # deck lengths match, check if content matches
#         left <- decks[[1]]$as_list()
#         for (l in seq_along(left)) {
#           if (left[[l]] != d[[1]][[l]]) {
#             match <- FALSE
#             break
#           }
#         }
#         if (match) {
#           # deck 1 matches, check deck 2
#           left <- decks[[2]]$as_list()
#           for (l in seq_along(left)) {
#             if (left[[l]] != d[[2]][[l]]) {
#               match <- FALSE
#               break
#             }
#           }
#         }
#         if (match) {
#           # player 1 wins this game.
#           cat("hit a match in our decks at round", rounds, "i=", i, "\n")
# #          for (p in prev_decks) {
# #            cat("decks:", p[[1]] |> unlist(), p[[2]] |> unlist(), "\n")
# #          }
# #          cat("match:", decks[[1]]$as_list() |> unlist(), decks[[2]]$as_list() |> unlist(), "\n")
# #          cat("match at:", i, "\n")
#           return(list(winner=1, deck=decks[[1]]))
#         }
#       }
#     }
    # add our decks
#    prev_decks[[length(prev_decks)+1]] <- list(decks[[1]]$as_list(), decks[[2]]$as_list())
  }
  return(NULL)
}

decks <- tibble(input, player = cumsum(str_detect(input, "Player"))) |>
  filter(!str_detect(input, "Player"), input != "") |>
  group_by(player) |>
  mutate(input = as.integer(input)) |>
  summarise(input = list(input)) |>
  deframe() |> map(as.list)

#library(memoise)
#play_game <- memoise(play_game)
out <- play_game(decks)

winning_cards <- out$deck$as_list() |> unlist()

(winning_cards*rev(seq_along(winning_cards))) |>
  sum()

