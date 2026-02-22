library(tidyverse)
library(igraph)

input = "Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
Valve BB has flow rate=13; tunnels lead to valves CC, AA
Valve CC has flow rate=2; tunnels lead to valves DD, BB
Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
Valve EE has flow rate=3; tunnels lead to valves FF, DD
Valve FF has flow rate=0; tunnels lead to valves EE, GG
Valve GG has flow rate=0; tunnels lead to valves FF, HH
Valve HH has flow rate=22; tunnel leads to valve GG
Valve II has flow rate=0; tunnels lead to valves AA, JJ
Valve JJ has flow rate=21; tunnel leads to valve II" |>
  str_split("\n") |> unlist()

input <- readLines("2022/day16/input.txt")
input <- tibble(input = input) |>
  extract(input, into=c('valve', 'rate', 'tunnels'), regex="Valve ([A-Z]+) has flow rate=([0-9]+); tunnels? leads? to valves? (.*)",
          convert=TRUE) |>
  mutate(tunnels = str_split(tunnels, ", "))

# ok, figure out the network
graph <- input |> select(valve, tunnels) |>
  unnest(tunnels) |>
  as.matrix() |>
  graph_from_edgelist(directed=FALSE) |>
  set_vertex_attr(name='size', index = input$valve, value = input$rate) |>
  set_edge_attr(name='weight', value = 1)

# ok, find the distance between non-stuck valves to simplify the graph
valves_to_consider <-
  input |> filter(rate > 0 | valve == "AA") |>
  select(valve, rate) |>
  arrange(valve)

graph |>
  distances(v = valves_to_consider$valve,
            to = valves_to_consider$valve)

# we have basically a TSP style problem? Visit as
# many nodes as possible starting at AA such that the
# sum of the edge weight and the pressures is minimised.
# I think this is the travelling purchaser problem.

# It's NP-hard, but this is a small(ish)
# network so is probably memoise-able via a DFS style
# approach?

# We could reduce the network and work on it directly,
# alternatively just note we can step from valve to valve
# directly on a complete graph, noting that when we get
# to a valve we must open it (i.e. order of valves open is
# what matters).

# i.e. it's an optimal permutation of valves.

# grab our distances. Add 1 so that we open the valve directly.
# that way the added pressure is computed as time_remaining*pressure.
dist <- graph |>
  distances(v = valves_to_consider$valve,
            to = valves_to_consider$valve)
dist <- dist+1
valves <- logical(nrow(dist))
valves[1] <- TRUE

pressure <- valves_to_consider$rate

get_max_pressure <- function(dist, pressure, valve, time_remaining, open_valves) {
#  cat("At valve", valve, "with", time_remaining, "to go, cur_pressure=", cur_pressure, "\n")
  # hmm, exit condition is based on remaining time.
  if (time_remaining <= 0)
    return(0)

  # open the valve
  open_valves[valve] <- TRUE

  # add to our pressure
  valve_pressure <- pressure[valve]*time_remaining # we've already opened the valve

  # iterate over unopen valves
  max_pressure <- 0
  for (i in which(!open_valves)) {
    if (dist[valve, i] <= time_remaining) {
      next_pressure = get_max_pressure(dist, pressure, i, time_remaining - dist[valve,i], open_valves)
      if (next_pressure > max_pressure)
        max_pressure = next_pressure
    }
  }
  return(valve_pressure + max_pressure)
}


# ok memoise this?
library(memoise)
get_max_pressure <- memoise(get_max_pressure)
get_max_pressure(dist, pressure, which(rownames(dist) == 'AA'), 30, valves) # yay!

# lol, memoisation makes things slower :(

# ok in part 2 we have 26 minutes but two steppers! Fun!

# I guess we need to track two time_remaining's AND two current valves.
# then it's just iterating across all pairs of unopened valves (in each order?)
# and checking them in turn?

# what about subsetting it? There's 2^15 subsets, right (noting they both need AA)
library(sets)
all_sets <- set_power(1:(nrow(dist)-1)) |> as.list() # due to symmetry this is twice as much as needed, but OK

get_max_pressure_for_subgraph <- function(set, time_remaining) {
  sub_dist <- dist[c(1, set+1), c(1, set+1), drop=FALSE]
  sub_pressure <- pressure[c(1, set+1)]
  sub_valves <- valves[c(1, set+1)]
  get_max_pressure(sub_dist, sub_pressure, 1, time_remaining, sub_valves)
}

# ok, for each one, we subset the two and run the part 1
max_dist <- 0
for (i in seq_along(all_sets)) {
  # empty set
  set1 <- all_sets[[i]] |> as.integer()
  set2 <- setdiff(1:(nrow(dist)-1), set1)
#  if (length(set1) != 5 && length(set2) != 5)
#    next # don't bother
  cat("working on set", set1, "(", i, "of", length(all_sets), ")\n")
  # ok, add in
  dist1 <- get_max_pressure_for_subgraph(set1, 26)
  dist2 <- get_max_pressure_for_subgraph(set2, 26)
  if (max_dist < dist1 + dist2)
    max_dist = dist1 + dist2
}

