library(tidyverse)

d <- read_lines('day3/input.txt')

# regex for part 1, plus mayaswell use R to evaluate the product as a function
# via eval()
d |> str_extract_all("mul\\([[:digit:]]{1,3},[[:digit:]]{1,3}\\)") |>
  unlist() |>
  str_replace_all("mul", "prod") |>
  map(str2expression) |>
  map_dbl(eval) |>
  sum()

# part 2 we need to drop anything between a don't() and do(). Presumably
# we can just filter those out?? i.e. filter out everything from don't()...do()
# via a regex

# hmm, we might have a do/don't crossing a line break, so let's paste first

d |> paste(collapse="") |>
  str_remove_all("don't\\(\\).*?do\\(\\)") |>
  str_remove_all("don't\\(\\).*") |>  # capture the last don't...
  str_extract_all("mul\\([[:digit:]]{1,3},[[:digit:]]{1,3}\\)") |>
  unlist() |>
  str_replace_all("mul", "prod") |>
  map(str2expression) |>
  map_dbl(eval) |>
  sum()
