library(tidyverse)

input <- "Player 1 starting position: 4
Player 2 starting position: 8" |> str_split('\n') |> unlist()
input <- readLines('2021/day21/input.txt')
in_position <- tibble(input = input) |>
  extract(input, into='position', regex=": ([0-9]+)", convert=TRUE) |>
  pull(position)

# ok, now roll our die over and over, moving around and adding scores
position <- in_position
scores <- rep(0, 2)
die <- 1
player <- 1
total_rolls <- 0
while(all(scores < 1000)) {
  # player rolls the die
  rolls <- (die + 0:2 - 1) %% 100 + 1
  die <- (die + 3 - 1) %% 100 + 1
  total_rolls <- total_rolls + 3
  new_pos <- (position[player] + sum(rolls) - 1) %% 10 + 1
  position[player] <- new_pos
  scores[player] <- scores[player] + new_pos
  player <- 3-player # switch player
}

# score is the loser times total rolls
total_rolls * min(scores)

# part 2: now we need to know how often the player wins in each universe
# so each roll splits things 3 ways. We can't really actually run
# them across this universe? Memoise maybe?
# inputs would be our current position and scores, so 21*21*10*10*2 which
# isn't too bad, right?
# it would return the number of games each player won
count_wins <- function(position, scores, player) {
  if (any(scores >= 21)) {
    return(ifelse(scores >= 21, 1, 0)) # a player wins
  }
  # player rolls the dirac dice 3 times. Each time we have to branch three times
  total_wins <- c(0, 0)
  for (roll1 in 1:3) {
    for (roll2 in 1:3) {
      for (roll3 in 1:3) {
        move = roll1 + roll2 + roll3
        # update our player position
        new_position <- position
        new_scores   <- scores
        new_position[player] <- (position[player] + move - 1) %% 10 + 1
        new_scores[player] <- scores[player] + new_position[player]
        new_player <- 3 - player
        # branch for a new universe
        total_wins <- total_wins + count_wins(new_position, new_scores, new_player)
      }
    }
  }
  return(total_wins)
}

library(memoise)
count_wins <- memoise(count_wins)

position <- in_position
scores   <- c(0, 0)
player   <- 1
ans <- count_wins(position, scores, player)

