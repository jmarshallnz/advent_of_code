library(tidyverse)

input <- readLines('2020/day25/input.txt') |> as.integer()

# Set the value to itself multiplied by the subject number.
# Set the value to the remainder after dividing the value by 20201227.

public_key <- function(loops, secret = 7, ret_all=TRUE) {
  keys <- integer(loops)
  key <- 1
  for (i in 1:loops) {
    key <- key * secret
    key <- key %% 20201227
    keys[i] <- key
  }
  if (ret_all) keys else key
}

# I'm guessing that 20201227 is prime
# Yep:
sum(floor(20201227 / seq(1:sqrt(20201227)))^2 == 20201227)

# Either way the number of keys can't be more than 20201227
all_keys <- public_key(20201227)

card_key <- input[1]
card_loop <- which(all_keys == card_key)

door_key <- input[2]
door_loop <- which(all_keys == door_key)
door_loop

public_key(door_loop, card_key, ret_all=FALSE)
public_key(card_loop, door_key, ret_all=FALSE)
