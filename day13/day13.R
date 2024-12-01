library(tidyverse)

input <- read.table(text="Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279
", sep="", fill=TRUE) |> apply(1, paste0, collapse=' ')

input <- readLines('day13/input.txt')

inputAB <- input[c(TRUE,TRUE,FALSE,FALSE)]
inputZ <- input[c(FALSE,FALSE,TRUE,FALSE)]

parsedAB <- tibble(inputAB) |>
  extract(inputAB, into=c("button","x","y"), regex=".*?([AB])\\: X\\+([0-9]+), Y\\+([0-9]+)", convert=TRUE) |>
  mutate(arcade = rep(1:(n()/2), each=2))

parsedZ <- tibble(inputZ) |> extract(inputZ, into=c("x", "y"), regex="X=([0-9]+), Y=([0-9]+)", convert=TRUE) |>
  rowid_to_column('arcade')

in_mat <- parsedAB |> arrange(button) |>
  select(-button) |>
  group_nest(arcade) |>
  pull(data) |>
  map(as.matrix)

in_ans <- parsedZ |> group_nest(arcade) |> pull(data) |> map(as.matrix)

map2(in_mat, in_ans, \(x, y) solve(t(x), t(y))) |>
  map_dbl(\(x) if (all(abs(round(x) - x) < 1e-10 & x <= 100)) { sum(c(3,1)*x) } else 0) |>
  sum()

# part 2
in_ans <- in_ans |>
    map(\(x) x + 10000000000000)
ans <- map2(in_mat, in_ans, \(x, y) solve(t(x), t(y))) |>
  map(round)

# which ones count?
wch <- ans |>
  map2(in_mat, \(x, y) t(y)%*%x) |>
  map2(in_ans, \(x, y) t(y)-x) |>
  map_lgl(\(x) all(x == 0))

ans_wch <- ans[wch] |> reduce(`+`)

options(scipen=999)
sum(ans_wch*c(3,1))
