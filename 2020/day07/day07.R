library(tidyverse)

input <- readLines('2020/day07/input.txt')

bags <- tibble(input = input) |>
  extract(input, into=c('bag', 'subbags'), regex="([a-z ]+) bags contain (.*)\\.") |>
  mutate(splitsubs = str_split(subbags, pattern=", ")) |>
  unnest(splitsubs) |>
  extract(splitsubs, into=c('subcount', 'subbag'), regex="([0-9]+) (.*) bags*", convert=TRUE)

# OK, now we can basically 'expand' our subbags progressively
bags_left <- bags
bags_list <- tibble(bag = 'shiny gold')
while (TRUE) {
  # join to our bags list
  cat("bag list is now", nrow(bags_left), "\n")
  more_bags <- bags_left |> semi_join(bags_list, join_by(subbag == bag))
  if (more_bags |> nrow() == 0)
    break
  bags_list <- bind_rows(bags_list, more_bags)
  bags_left <- bags_left |> anti_join(more_bags)
}
bags_list |> summarise(n_distinct(bag)-1)

# Part 2: Start with shiny_gold and keep adding inside it
bags_left <- bags |> filter(subbag != "no other bags")
bags_list <- tibble(subbag = 'shiny gold', subcount=1)

while (TRUE) {
  # join to our bags list
  cat("bag list is now", nrow(bags_list), "\n")
  more_bags <- bags_list |> select(bag = subbag, count=subcount) |> inner_join(bags_left, by='bag') |>
    select(bag, count, subcount, subbag)
    mutate(subcount = count*subcount) |>
    group_by(subbag) |> summarise(subcount = sum(subcount))
  # OK, now accumulate up
  if (more_bags |> nrow() == 0)
    break
  bags_left <- bags_left |> anti_join(bags_list, join_by(bag == subbag)) # just take the GOLD out
  bags_list <- bind_rows(bags_list, more_bags)
}

bags_list |> summarise(sum(subcount)-1)

# We have cycles by the looks? Or at least multiple ways to meet the same (sub) bag.
# So we're going to have to build the list up directly I think?

bag_list <- bags |> filter(subbag != "no other bags") |>
  group_by(bag) |> group_split()
bag_list <- bag_list |> set_names(nm=map_chr(bag_list, \(x) x |> pull(bag) |> unique())) |>
  map(\(x) x |> select(subcount, subbag))

# OK, now start at "solid gold" and traverse
num_subbags <- function(bag) {
  if (is.null(bag_list[[bag]]))
    return(1) # just 'bag' left
  total <- 1 # this bag
  # add up our bags
  cat("checking bag", bag, "\n")
  for (i in 1:nrow(bag_list[[bag]])) {
    subbag <- bag_list[[bag]][i,]
    total <- total + subbag$subcount * num_subbags(subbag$subbag)
  }
  cat("total in bag", bag, ":", total, "\n")
  return(total)
}

num_subbags('shiny gold')-1 # exclude the shiny gold one
