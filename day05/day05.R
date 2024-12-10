library(tidyverse)

# part 1
d <- read_lines("day5/input.txt")
order <- d[str_detect(d, "\\|")] |>
  str_split_fixed("\\|", n=2) |>
  as.data.frame() |>
  mutate(across(everything(), as.integer))

printing <- d[str_detect(d, ",")] |>
  str_split(",") |>
  enframe() |>
  unnest(value) |>
  group_by(name) |>
  mutate(order = row_number()) |>
  mutate(value = as.integer(value))

# OK, we need to see which printing satisfies the orders. One way is to extract
# all ordered pairs in each printing and check they are the same order as the ordered
# ones via a join. Not very efficient, but who cares?

# we need only find which ones don't satisfy a rule, which can be done by reversing
# the rules and then semi_join()'ing (return rows that break the rules).

broken <- printing |> full_join(printing, by='name', relationship='many-to-many') |>
  filter(order.x < order.y) |>
  semi_join(order, by = join_by(value.x == V2, value.y == V1)) |>
  select(name) |>
  unique()

# grab the unbroken ones, pull out the middle entry, and sum
printing |> anti_join(broken) |>
  group_by(name) |>
  arrange(order) |>
  slice((n()+1)/2) |>
  ungroup() |>
  summarise(sum(value))

# Part 2

# We need to reorder the broken ones. This could be done with
# a linear program? Constraints on order and need to optimise
# the position?
#
# Alternatively, we can rank each observation by how often it
# appears in the order list on the left (conditional on another
# entry also being in the list:
printing |> semi_join(broken) |>
  left_join(order, by=join_by(value == "V1"), relationship='many-to-many') |>
  group_by(name) |>
  filter(V2 %in% value) |> # need to have V2 be in the same set of numbers
  group_by(name, value, order) |>
  summarise(n = n()) |>
  group_by(name) |>
  arrange(name, desc(n)) |>
  slice(n()/2 + 1) |> # we would have lost the last one
  ungroup() |>
  summarise(sum(value))
