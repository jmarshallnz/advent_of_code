library(tidyverse)
options(scipen=999)

input <- "r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb" |> str_split("\n") |> unlist()

towels <- input[1] |> str_split(', ') |> unlist()

designs <- input[-c(1:2)]

# need to know how many designs are possible.
# this is an ordering problem I think? e.g.
# bggr could be made with b, g, g, r in that order
# or bg, g, r
# or bgg, r
# or bg, gr
# or b,g,gr
# or b,ggr
# etc.

# in fact, the number of possibilities is the sum
# of positive integers has to equal 4. i.e. we need
# ordered partitions of sets?
#library(partitions)

# This would work, but the problem is the input is super long,
# so doing it this way wouldn't work very nicely at all as the
# number of partitions is going to grow very quickly indeed.
# instead, it seems the way to do it would be some sort
# of search.

# Hmm, I wonder if we could use paths on a graph?
# i.e. use a complete graph of our inputs (there's
# WAY too many of them maybe?)
# and then check if the path can be broken down that way?
# by the looks the biggest towel we have is 8 characters
# and the smallest 2. As we need only know whether we
# have a match for part 1 we can probably just do a DFS?

input <- read_lines("day19/input.txt")
towels <- input[1] |> str_split(', ') |> unlist()
designs <- input[-c(1:2)]
# how many hit each other? i.e. how many do we need
# to consider at each step?
design_hits <- expand_grid(t=towels, d=designs) |>
  group_by(t) |>
  filter(str_detect(d, fixed(first(t)))) |>
  group_by(d)

possible_towels <- design_hits |>
  group_split() |>
  map(pull, t) |>
  map(str_split, pattern="")

designs <- design_hits |> group_keys() |> pull(d) |>
  str_split("")

all_towels <- map(towels, str_split, pattern="") |> map(unlist, recursive=FALSE)

# only 5 hit each design at the start, so our branching probably
# won't be too bad
hits_left <- function(design, towels) {
#  cat('calling hits left with design ', design, '\n')
#  print(towels)
  if (length(design) == 0)
    return(TRUE)
  for (i in 1:length(towels)) {
    ch = seq_along(towels[[i]])
    if (length(design) >= length(towels[[i]]) && all(design[ch] == towels[[i]])) {
      # hit the left part of it, so check the rest
      if (hits_left(design[-ch], towels))
        return(TRUE)
    }
  }
  return(FALSE)
}

wch_designs <- map2_lgl(designs, possible_towels, hits_left)
sum(wch_designs)

# ok, for part two we need the NUMBER of ways to do it. To do this we need to count them all.
# I think we can take our current code and alter it to return the number of matches instead of whether it matches?
# i.e. if we have a match we increment by 1?

designs <- designs[wch_designs]
possible_towels <- possible_towels[wch_designs]

# We can't use the above style of code for this - way too many possibilities to traverse.
# Instead, we could reduce the pool a whole heap by just seeing where each possible towel
# can fit. i.e. a position->list(towels) map.
# in fact, we only need a position->list(towel_length) map.
# that way we have a position->list(next_position) map which then
# forms a system of equations for N(design[position:]) that we can solve either
# in reverse by just walking back, or just setting up the matrix and hitting solve()

# it's going to be easier to deal with strings in this case
possible_towels_str <- possible_towels |> map( \(x) map(x, paste0, collapse=''))
designs_str <- designs |> map(paste0, collapse='')

count_designs <- function(design_str, towel_str) {

  cat('counting design ', design_str, '\n')

#  positions_hit <- function(towel, design) {
#    str_locate_all(design, pattern=fixed(towel)) |>
#      map(as.data.frame) |> map(pull, start) |>
#      unlist()
#  }

  # str_locate_all() doesn't hit overlapping sequences, so we need our own for
  # this. This is a lot easier with vectors I think? Just shift things along
  positions_hit2 <- function(towel, design_str) {
    subseq <- seq_len(nchar(design_str)-nchar(towel)+1) |>
      map_chr(\(x) str_sub(design_str, x, x+nchar(towel)-1))
    which(subseq == towel)
  }

  poss <- map(towel_str, positions_hit2, design_str) |>
    set_names(towel_str) |>
    enframe() |>
    unnest(value) |>
    complete(value=1:nchar(design_str)) |> # may not hit every position ofc!
    group_split(value) |>
    map(pull, name)

  # we need to know how many there are now. To do this we have to multiply up the potentials
  # basically all we need is the lengths in each one now I think. We don't care about anything
  # else.
  hitting_lengths <- poss |> map(nchar)

  # essentially from this we have a set of simultaneous equations to solve.
  # though it's presumably trivial just to solve it backwards?

  # I think probably backwards?
  foo <- hitting_lengths |> enframe() |> unnest(value) |> mutate(ref = name+value) |>
    arrange(desc(name)) |>
    na.omit()
  # ok, drop this into a matrix equation
  mat <- matrix(0, nrow=nchar(design_str), ncol=nchar(design_str))
  mat_pos <- foo |> filter(ref != max(ref)) |>
    select(name, ref) |> as.matrix()
  vec_pos <- foo |> filter(ref == max(ref)) |>
    pull(name)

  mat[mat_pos] <- 1
  vec <- numeric(nchar(design_str))
  vec[vec_pos] <- 1

  # ok, matrix formula is: n = mat*n + vec, so solve that and pull the top entry:
  solve(mat - diag(1, nrow(mat), ncol(mat)), -vec)[1]
}

ans <- map2_dbl(designs_str, possible_towels_str, count_designs)
sum(ans)

# Alternate solution is counting paths in a graph, right?
# in fact, we need only the adjacency matrix for this
count_paths <- function(design, towels) {
  len <- nchar(design)+1
  vertices <- tibble(id = seq_len(len))

  edges <- vertices |> cross_join(vertices) |>
    filter(id.x < id.y) |>
    mutate(match = map2_chr(id.x, id.y, \(x, y) str_sub(design, x, y-1))) |>
    semi_join(tibble(match=towels |> unlist()), by='match')

  # fill in adjacency matrix
  adj <- matrix(0, nrow=len, ncol=len)
  adj[edges |> select(id.x, id.y) |> as.matrix()] <- 1

  # count the paths to the end
  solve(diag(1, nrow(adj), ncol(adj))-adj)[1,len]
}

ans2 <- map2_dbl(designs_str, possible_towels_str, count_paths)
sum(ans2)

# draw a graph to visualise it
library(igraph)
design <- designs[[6]]
towels <- possible_towels[[6]]

len <- nchar(design)+1
vertices <- tibble(id = seq_len(len))

edges <- vertices |> cross_join(vertices) |>
  filter(id.x < id.y) |>
  mutate(match = map2_chr(id.x, id.y, \(x, y) str_sub(design, x, y-1))) |>
  semi_join(tibble(match=towels |> unlist()), by='match')

g <- graph_from_edgelist(edges |> select(id.x, id.y) |> as.matrix()) |>
  set_edge_attr('label', value=edges |> pull(match))

coords <- matrix(c(0:6*2, rep(0,7)), ncol=2)

library(ggraph)
ggraph(g, layout=coords) +
  geom_edge_arc(aes(label=label),
                angle_calc = 'along',
                label_dodge = unit(2.5, 'mm'),
                #arrow = arrow(length = unit(4, 'mm')),
                start_cap = circle(3, 'mm'),
                end_cap = circle(3, 'mm'),
                strength=1.2) +
  geom_node_point(size = 5) +
  coord_fixed(xlim=c(-1,7*2-1), ylim=c(-1,2.5)) +
  theme_void()

# part 2 memoised. Damn, this is neat
library(memoise)

count_paths <- function(design, towels) {
#  cat('calling hits left with design ', design, '\n')
  #  print(towels)
  if (length(design) == 0)
    return(1)
  num_patterns <- 0
  for (i in 1:length(towels)) {
    ch = seq_along(towels[[i]])
    if (length(design) >= length(towels[[i]]) && all(design[ch] == towels[[i]])) {
      # hit the left part of it, so check the rest
      num_patterns <- num_patterns + count_paths(design[-ch], towels)
    }
  }
  return(num_patterns)
}

count_paths <- memoise(count_paths)
ans3 <- map2_dbl(designs, possible_towels, count_paths)
sum(ans3)

