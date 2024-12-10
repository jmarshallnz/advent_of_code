library(tidyverse)

# part 1

d <- read_table("day1/input.txt", col_names = FALSE)

d |> mutate(across(everything(), sort)) |>
  summarise(sum(abs(X1-X2)))

# part 2

d |> select(X1) |>
  left_join(d |> count(X2),
            join_by(X1 == X2)) |>
  replace_na(list(n=0)) |>
  summarise(sim = sum(X1*n))
