library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

directions <- readLines("2023/day08/input.txt")[1] |>
  str_split('') |> unlist()

network <- read.table("2023/day08/input.txt", skip=2) |>
  extract(V3, into='L') |>
  extract(V4, into='R') |>
  select(-V2) |>
  tibble::column_to_rownames("V1") |>
  as.matrix()

# iterate over our directions, counting the number of moves
node <- "AAA"
dir_count <- 0
while(node != "ZZZ") {
  node <- network[node, directions[dir_count %% length(directions) + 1]]
  dir_count <- dir_count+1
  cat(dir_count, "\n")
}
count_trip("AAA", "ZZZ")

# part2: same but repeat with every node starting/ending with A, and
# need to check for cycles thereof.
# likely going to be some lowest common multiplier shenanigans, noting that 307 is prime.

count_trip <- function(start_node, end_node, max_dir = 800^2) { # we MUST hit it in this time as pigeon hole principle, right?
  node <- start_node
  dir_count <- 0
  while(node != end_node) {
    node <- network[node, directions[dir_count %% length(directions) + 1]]
    dir_count <- dir_count+1
    if (dir_count %% 100000 == 0)
      cat(dir_count, "\n")
    if (dir_count > max_dir)
      break
  }
  return(dir_count)
}

start_nodes <- rownames(network)[str_detect(rownames(network), ".*A$")]
end_nodes   <- rownames(network)[str_detect(rownames(network), ".*Z$")]

# ok, now see how long it takes to go from each to each I guess?
trip_counts <- expand_grid(start=start_nodes, end=end_nodes) |>
  mutate(count = map2_dbl(start, end, count_trip)) |>
  filter(count < 800*800)

# ok, now it's just going to be the lcm of our counts
library(pracma)
trip_counts |>
  summarise(reduce(count, Lcm))
