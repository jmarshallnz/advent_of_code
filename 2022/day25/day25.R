library(tidyverse)

input <- '1=-0-2
12111
2=0=
21
2=01
111
20012
112
1=-1=
1-12
12
1=
122' |> str_split("\n") |> unlist()

input <- readLines('2022/day25/input.txt')

digits <- c(`2` = 2, `1` = 1, `0` = 0, `-` = -1, `=` = -2)
snafu <- c(`0` = '0', `1` = '1', `2` = '2', `3` = '=', `4` = '-')

# convert our SNAFU to decimal (easy)
snafu_to_decimal <- function(s) {
  d <- digits[str_split(s, '') |> unlist()]
  sum(d * 5^rev(seq_along(d)-1))
}

# convert decimal to SNAFU (little bit trickier)
decimal_to_snafu <- function(d) {
  len <- ceiling(logb(d, base = 5))+1 # possibly need an extra digits
  # ok, now progressively divide out from smallest to largest (or largest to smallest?)
  out <- character(len)
  for (i in 1:len) {
    s <- snafu[d %% 5 + 1] # 1..5
    out[len-i+1] <- s
    d <- (d - digits[s]) / 5
    if (d == 0)
      break
  }
  paste(out, collapse='')
}

d <- map_dbl(input, snafu_to_decimal) |>
  sum()
d
decimal_to_snafu(d)
