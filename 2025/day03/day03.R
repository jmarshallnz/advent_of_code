library(tidyverse)

input <- readLines("2025/day03/input.txt") |>
  str_split_fixed('', n=Inf) |>
  apply(2, as.numeric) |>
  t() |>
  as.data.frame() |> as.list()

# part 1:
first <- map(input, \(x) { which.max(x[-length(x)]) })
map2_dbl(input, first, \(x, y) { x[y]*10 + max(x[-c(1:y)])}) |>
  sum()

# part 2 works identically but we just need to iterate...
pos <- rep(1, length(input))
codes <- rep(0, length(input))
for (i in 11:0) {
  digit_pos <- map2_dbl(input, pos, \(x, y) which.max(x[y:(length(x)-i)])+y-1)
  codes <- codes*10 + map2_dbl(input, digit_pos, \(x, y) x[y])
  pos <- digit_pos+1
}
options(digits = 22)
sum(codes)
