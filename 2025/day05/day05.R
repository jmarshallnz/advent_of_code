library(tidyverse)

raw <- readLines("2025/day05/input.txt")

ranges <- raw[1:(which(raw == "")-1)] |> str_split_fixed("-", n=2) |>
  as.data.frame() |>
  mutate(across(everything(), as.numeric))
ids <- data.frame(id = as.numeric(raw[-(1:which(raw==""))]))

valid <- ids |>
  cross_join(ranges) |> # could probably do a more clever left join or semi join?
  filter(id >= V1, id <= V2) |>
  select(id) |> unique()

valid |> nrow()

# part 2 we need only figure out the total fresh. i.e. union up the ranges and count their total
# size. One way to do this is to do 1-intersection(!ranges) which might be easier?
# we could maintain a list of "good" ranges, then it's just an intersection

max_id <- max(ranges$V2)
valid_ranges <- list(c(1,max_id))

# ok, now iterate through the list of !ranges - we have two for each
inverses <- map2(ranges$V1, ranges$V2, \(x, y) data.frame(l=c(1, y+1), r=c(x-1, max_id))) |>
  map(\(x) filter(x, r > 1, l < max_id))

# ok, intersect our current valid_ranges with our inverses
intersect <- function(ranges, range) {
  ranges |> cross_join(range) |>
    mutate(l = pmax(l.x, l.y),
           r = pmin(r.x, r.y)) |>
    filter(l <= r) |>
    select(l, r)
}

final_inverse <- reduce(inverses, intersect)

# ok, the number of "fresh" is then the ivnerse of this
final_inverse |>
  mutate(length = r-l+1) |>
  summarise(max_id - sum(length))
