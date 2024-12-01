library(tidyverse)

input <- read_lines('day11/input.txt') |>
  str_split_fixed(' ', n=Inf) |>
  as.numeric()

# recognise that we can just duplicate stones. i.e. need only hold the
# number of stones of each number

stones <- tibble(number = input, rep = rep(1,length(input)))

# ok, do an update
update_rule <- function(number) {
  if (number == 0) {
    return(1)
  }
  chnum <- as.character(number)
  if (nchar(chnum) %% 2 == 0) {
    dig_right <- 10^(nchar(chnum)/2)
    return(c(number %/% dig_right,
             number %% dig_right))
  }
  2024*number
}

update_stones <- function(stones) {
  stones |> mutate(number = map(number, update_rule)) |>
    unnest(number) |>
    group_by(number) |> summarise(rep = sum(rep))
}

for (i in 1:25) {
  stones <- update_stones(stones)
}

sum(stones$rep)

# part 2 -  another 50 times
for (i in 1:50) {
  stones <- update_stones(stones)
}

sum(stones$rep)
