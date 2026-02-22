library(tidyverse)

options(digits = 22,
        pillar.max_dec_width = 22)

input <- '1,20,11,6,12,0' |>
  str_split(",") |> unlist() |> as.integer()

# OK, for part 1 we don't care about the players
plays <- integer(2020)
plays[seq_along(input)] <- input
for (i in seq(length(input)+1, 2020)) {
  last_spoken = plays[i-1]
  # has this been spoken before?
  wch <- which(plays[1:(i-2)] == last_spoken)
  if (length(wch)) {
    # yes: play the difference between i-1 and whenever it was LAST spoken
    plays[i] <- i-1 - last(wch)
  } else {
    # no: play a zero
    plays[i] <- 0
  }
}
plays[2020]

# Part 2: determine the 30000000th number. This won't be efficient with the above,
# but we note we really need only remember the last time each number was spoken.
library(sparsevctrs)

# use a sparse vector?
predone <- tibble(where=seq_along(input[-length(input)]), input=input[-length(input)]+1) |> arrange(input)

last_plays <- sparse_integer(values = predone$where, positions = predone$input, length = 30000000)
# OK, now play the game...
last_spoken <- input[6]
for (i in 7:30000000) {
  if (i %% 1000000 == 0) {
    cat('up to', i, '\n')
  }
  # fine the time that last_spoken was spoken
  last_played <- last_plays[last_spoken+1]
  if (last_played > 0) { # have played before, use the difference between i-1 and when it was last played
    next_play <- (i-1) - last_played
  } else { # nope, play a zero
    next_play <- 0
  }
  # do the play
  last_plays[last_spoken+1] <- i-1 # last played at time i-1
  last_spoken <- next_play
}
next_play
