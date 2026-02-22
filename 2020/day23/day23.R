library(tidyverse)
library(collections)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- '389125467' |> str_split('') |> unlist() |> as.integer()
input <- '368195742' |> str_split('') |> unlist() |> as.integer()

run_game <- function(input, num_rounds) {
  min_label <- min(input)
  max_label <- max(input)
  get_next_cups <- function(x) {
    pos <- which(x == 1) + c(1,2)
    pos[pos > length(input)] <- pos[pos > length(input)]-length(input)
    return(x[pos])
  }
  cups <- input
  history <- list()
  for (rounds in 1:num_rounds) {
    # pickup the next 3 cups:
    current <- cups[1]
    pickup <- cups[2:4]; cups <- cups[-c(2:4)]
    # grab the destination cup
    dest_label <- current-1
    dest_pos <- 0
    while (TRUE) {
      wch <- which(cups == dest_label)
      if (length(wch) == 1) {
        # found
        dest_pos <- wch
        break
      }
      dest_label <- dest_label-1
      if (dest_label < min_label) dest_label = max_label
    }
    # insert the cups
    cups <- c(cups[seq_len(dest_pos)], pickup, cups[dest_pos+seq_len(length(cups)-dest_pos)])
    # rotate our input
    cups <- c(cups[2:length(cups)], current)
    history[[length(history)+1]] <- list(left=cups[1:10], nx=get_next_cups(cups))
  }
  return(lst(cups, history))
}

cups <- run_game(input, 100)$cups

pos <- which(cups == 1)
c(cups[pos+seq_len(length(cups)-pos)], cups[seq_len(pos-1)]) |>
  paste(collapse='')

# Part 2: Now we have 1,000,000 cups and 10,000,000 moves. Am guessing that the
# above is probably too slow to brute-force.

# We don't need the entire order. All we need to know is which two cups end up
# immediately to the right of cup 1 at the end of 10,000,000 moves.

# Let's take a look at the first 100 odd moves of case 1

run_game(input, 100)$history |> list_transpose() |>
  set_names(c('p1', 'p2')) |>
  as.data.frame() |>
  tibble::rowid_to_column() |>
  pivot_longer(-rowid) |>
  ggplot() +
  aes(x=rowid, y=value, col=name) +
  geom_point()

# OK, I'm guessing the same thing will happen. How long will
# it take? Who knows...

long_input <- c(input, seq(max(input)+1, by=1, length.out=1000-length(input)))

test <- run_game(long_input, 10000)$history

test |> map('nx') |> list_transpose() |>
  set_names(c('p1', 'p2')) |>
  as.data.frame() |>
  tibble::rowid_to_column() |>
  pivot_longer(-rowid) |>
  ggplot() +
  aes(x=rowid, y=value, col=name) +
  geom_point()

test |> map('nx') |> list_transpose() |>
  set_names(c('p1', 'p2')) |>
  as.data.frame() |>
  count(p1, p2)

# after LOTS we get 8 6 still

# how long does it take to run 1 game of a million?
long_input <- c(input, seq(max(input)+1, by=1, length.out=1000000-length(input)))
system.time({
foo <- run_game(long_input, 1000)
})

# I think we need to know two properties:

# 1. For a given entry, which is the next item in the circle.
# 2. For a given entry, which is the previous item by label.
#
# On moving, what happens?
# 1. We get the next three items in the circle from number 1
# 2. And the _previous_ item by label from number 2 (up to next 3 times)
# 3. We update the next item by circle of our current to be item 4 (from 1)
# 4. We update the next item by circle of the _previous_ item by label (from 2) to be the first of the 3 removed.
# 5. We update the next item by circle of the 3rd moved to be the next item of the _previous_ item.

# I think we need:
# lab nex prv_lab
# 1   2.  9
# 2.  3.  1
# 3.  4.  2
# 4.  5.  3
# 5.  6.  4
# 6   7.  5
# ...
# 9.  1.  8

# MOVE:

# lab nex prv_lab
# 1   5.  9
# 2.  3.  1
# 3.  4.  2
# 4.  1.  3
# 5.  6.  4
# 6   7.  5
# ...
# 9.  2.  8

# CHANGES: current:next gets set to current:next+4 (iterate).
# OK, so we need a forward-linking list


# OK, now implement the game using this list
run_game <- function(input, moves, output=length(input)-1) {
  # create a linked list of the next item in the circle
  nex <- tibble(input) |>
    mutate(nx = lead(input, default=input[1])) |>
    arrange(input) |>
    pull(nx)

  curr <- input[1]
  for (i in 1:moves) {
    # print current results
#    print_nex <- accumulate(seq_len(length(nex)-1), \(x, y) nex[x], .init=curr) |>
#      unlist()
#    cat('now:', print_nex, '\n')
    # next three items to move
    mov1 <- nex[curr]
    mov2 <- nex[mov1]
    mov3 <- nex[mov2]
    # find the previous entry
    prev <- (curr-2) %% length(nex) + 1
    while(prev %in% c(mov1, mov2, mov3)) {
      prev <- (prev-2) %% length(nex) + 1
    }
    # do the move...
    nex[curr] <- nex[mov3]
    nex[mov3] <- nex[prev]
    nex[prev] <- mov1
    # next cup
    curr <- nex[curr]
  }
  # spit out the items to the right of cup 1
  accumulate(seq_len(output-1), \(x, y) nex[x], .init=nex[1]) |>
    unlist()
}

ans <- run_game(input, moves=100)
paste(ans, collapse='')

# OK, now see how long this runs for 1000000
long_input <- c(input, seq(max(input)+1, by=1, length.out=1000000-length(input)))
ans <- run_game(long_input, moves=10000000, output=2)
prod(ans)
