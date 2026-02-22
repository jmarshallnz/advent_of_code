library(tidyverse)

input <- "1
2
-3
3
-2
0
4" |>
  str_split("\n") |> unlist() |>
  as.integer()

input <- read.table("2022/day20/input.txt", header=FALSE) |> pull(V1)

# test if it's unique items (probably not?)
input |> length()
input |> unique() |> length() # NOPE.

# I think we want to maintain two lists/maps:
# The first is mapping from order to item.
# The second is mapping from item to order.

# Then when we move we update.

# Wrapping rule is a bit strange, as items are considered to be "between"
# other items. e.g. at position 3 if you have a -2 then you'd expect it to
# move to 1. But it will be considered to be between the items 7 and 1, and
# will be placed at 7, not at 1 (even though if placing it at 1 you'd still
# be correct circular!) So if the answer is 1 to a move LEFT we wrap to 7.
# Unclear what happens when going the other way? If you start at say 6 and
# have a move RIGHT one, what happens? Do you move to 7 or do you WRAP to 1?
# a b c d e 1 f -> a b c d e f 1 OR 1 a b c d e f? I think it would be the first?
# for consistency? A RIGHT 1 move is the same as a LEFT 6 move.

wrap_move <- function(cur, move, len=7) {
  # we normally can just increase our current position by the direction.
  pos <- (cur + move - 2) %% (len-1) + 2
  return(pos)
}

num_mixes <- 10
decryption_key <- 811589153

num_mixes <- 1
decryption_key <- 1

decrypted <- input*decryption_key

position_to_item <- seq_along(decrypted) # take items in this order?
item_to_position <- seq_along(decrypted) # the first item is in position 1 etc.

# process our items
for (i in 1:num_mixes) {
  cat("mixing number", i, "\n")
  for (wch_item in seq_along(decrypted)) { # process in original order
    if (wch_item %% 500 == 0)
      cat("slowly does it:", wch_item, "\n")
    # grab where this item currently is in our current order?
    wch_pos <- item_to_position[wch_item] # current position of this item
    item <- decrypted[wch_item] # current item from original order
    # move this item by that amount, wrapping as needed
    new_pos <- wrap_move(wch_pos, item, len=length(decrypted))
    #
    # removing the item from wch_pos will move everything to the RIGHT of wch_pos left one.
    # adding the item at next_pos will move everything to the RIGHT of next_pos right one.
    # TODO: Can optimise this a bit as a bunch won't actually move.
    # for now, we just implement this directly.
    # i.e. any items at positions more than wch_pos go down one
   # cat("moving item", wch_item, "which is", item, "from", wch_pos, "to", new_pos, "\n")
  #  cat("order was:", item_to_position, "positions were:", position_to_item, "\n")
  #  cat("sequence was:", input[position_to_item], "\n")
    temp_pos_to_item <- position_to_item[-wch_pos] # remove the position
    # add it back in
    new_pos_to_item <- c(temp_pos_to_item[seq_len(new_pos-1)], wch_item, temp_pos_to_item[new_pos-1+seq_len(length(temp_pos_to_item)-new_pos+1)])
    # check
    new_order <- decrypted[new_pos_to_item] # seems OK.
   ## cat("order now:", new_order, "\n")
    item_to_position <- order(new_pos_to_item) # seems OK?
    position_to_item <- new_pos_to_item
  }
}

# OK, now grove numbers:
grove <- function(sequence) {
  # find the position of "0"
  pos <- which(sequence == 0)
  # find the 1000th, 2000th and 3000th number wrapping our list as needed
  sequence[(pos + 1:3*1000 - 1) %% length(sequence) + 1]
}

decrypted[position_to_item] |> grove() |> sum()

# Part 2 is likely: Mix a bunch of times, where the above code will be way too slow?

# OK, it's not! It's basically the same but we multiply all the numbers first.
# Try on the baby example first
