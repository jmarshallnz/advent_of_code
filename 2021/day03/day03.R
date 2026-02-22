library(tidyverse)

input <- readLines('2021/day03/input.txt') |>
  str_split_fixed('', n=Inf)

ones <- input |> apply(2, \(x) sum(x == '1')) > nrow(input)/2

# convert to decimal
bin2dec <- function(b) {
  sum(2^rev(seq_along(b)-1)*b)
}
gamma <- bin2dec(ones)
epsilon <- bin2dec(!ones)
gamma*epsilon

# part 2
cur_oxy <- input
for (i in 1:ncol(input)) {
  count_1 <- sum(cur_oxy[,i] == '1')
  count_0 <- sum(cur_oxy[,i] == '0')
  # keep whichever is biggest
  cur_oxy = if (count_1 >= count_0) cur_oxy[cur_oxy[,i] == 1,,drop=FALSE] else cur_oxy[cur_oxy[,i] == 0,,drop=FALSE]
  if (nrow(cur_oxy) == 1) break
}
cur_co2 <- input
for (i in 1:ncol(input)) {
  count_1 <- sum(cur_co2[,i] == '1')
  count_0 <- sum(cur_co2[,i] == '0')
  # keep whichever is lowest
  cur_co2 = if (count_0 <= count_1) cur_co2[cur_co2[,i] == 0,,drop=FALSE] else cur_co2[cur_co2[,i] == 1,,drop=FALSE]
  if (nrow(cur_co2) == 1) break
}
dec_oxy <- bin2dec(cur_oxy == "1")
dec_co2 <- bin2dec(cur_co2 == "1")
dec_oxy*dec_co2
