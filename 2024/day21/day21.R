library(tidyverse)
library(igraph)

num_pad = read.table(text="789\n456\n123\n 0A", sep="\n") |>
  pull(V1) |> str_split_fixed("", n=Inf)

num_long <- num_pad |> as_tibble() |>
  rowid_to_column('row') |>
  pivot_longer(-row, names_to='col') |>
  extract(col, into='col', regex="([[:digit:]]+)", convert=TRUE) |>
  filter(value != " ") |>
  arrange(value) |>
  rowid_to_column('vertex')

num_edge <- num_long |> cross_join(num_long) |>
  filter((abs(row.x - row.y) == 1 & col.x == col.y) |
           (abs(col.x - col.y) == 1 & row.x == row.y)) |>
  mutate(dir = case_when(row.x < row.y ~ "v",
                         row.x > row.y ~ "^",
                         col.x < col.y ~ ">",
                         TRUE ~ "<")) |>
  select(vertex.x, vertex.y, dir)

graph <- graph_from_edgelist(num_edge |> select(vertex.x, vertex.y) |> as.matrix()) |>
  set_edge_attr("dir", value=num_edge |> pull(dir))

library(ggraph)

get_all_short_dirs <- function(graph, from) {
  get_short_dirs <- function(graph, from, to) {
    all_shortest_paths(graph, from=from, to=to)$epaths |>
    map(~edge_attr(graph, "dir", index=.)) |>
    map(paste0, collapse='') |> unlist()
  }
  map(V(graph), ~get_short_dirs(graph, from, to=.))
}

num_pad_dirs <- map(V(graph) |> as.integer(), ~get_all_short_dirs(graph, .))

# what are the minimal paths between each pair?
# we can probably figure that out later if we need it.
# don't seem to need it here, just a shortest path.

# let's think about this a bit more methodically. Is it possible
# for a sequence at step 1 to result in a shorter one at
# step 2 or even step 3 (or 4?) before we find one is superior?
# at higher steps it's all
# A{move1}A{move2}A etc
# so we if we can figure out the optimal moves between each button
# of the directional pad (if they exist?) then we should be able
# to just count the number of each move, as each of the
# moves at dirpad k will be represented by a larger set of the same
# moves at dirpad k+1. This should define a transistion
# matrix that we can use to iterate through robots.

# we note that on the dirpad going left is expensive as you
# always have to move from A and back again.

# noting that zigzagging to/from </A is inefficient then
# potential moves are:

#     A     ^     >     v     <
# A   -     <     v   <v,v<  v<<
# ^   >.    -   v>,>v   v    v<
# >   ^.  <^,^<   -     <    <<
# v ^>,>^   ^     >     -     <
# <  >>^    >^    >>    >     -

# we can improve this by seeing what happens if we use these
# combinations to various steps.
# given that we're forced to use v< to go from ^ to < and
# >^ to go from < to ^ my guess is these are more expensive.

# i.e. we should use <v where we can, and <^ where we can. This defines down left
# and up left (i.e. A -> v should be <v and > -> ^ should be <^).

# less clear is ^> vs >^ and v> vs >v

# for DR (down right)
# v>A gives <vA>A^A which gives   v<<A>A[UR]AvA^A<A>A
# >vA gives vA<A[UR]A which gives <vA[UR]Av<<A>>^A[[UR]]

# it's pretty clear from this that no matter what we use for [UR], >v is going
# to be longer than v> is. So we can conclude we should use v> for DR.

# for UR (up right), noting we have to go back to A after:

# ^>A gives <Av>A^A which gives   v<<A>>^A<vA>A^A<A>A
# >^A gives vA<^A>A which gives   <vA>^Av<<A>^A>AvA^A

# what about the next step?

# <vA<AA>>^AvAA<^A>Av<<A>A^>A<A>Av<<A>>^AvA^A
# v<<A>A>^AvA<^A>A<vA<AA>>^AvAv^A>AvA^A<vA>^A<A>A

# so after 3 steps we find that ^> is the best.

# so final matrix is:
dir_pad_dirs_order <- c("A", "^", ">", "v", "<")
dir_pad_dirs_mat <- matrix(c( "", "<", "v", "<v", "v<<",
                              ">", "" , "v>", "v", "v<",
                              "^", "<^", "", "<", "<<",
                              "^>", "^", ">", "", "<",
                              ">>^", ">^", ">>", ">", ""), nrow=5, byrow=TRUE)

# our transistion matrix thus has a small number of unique directions
# and any step up by there will be another combination of those
# unique directions
unique_dirs <- unique(dir_pad_dirs_mat |> as.vector()) # 15 unique directions

# ok, and now we need to know what happens when we transform each of these
# with the directional pad
get_next_dir_pad_code <- function(code) {
  code <- c("A", code, "A")

  dir_order <- tibble(order=seq_along(dir_pad_dirs_order),
                      value=dir_pad_dirs_order)

  code_lu <- tibble(value=code) |>
    left_join(dir_order) |> pull(order)

  next_codes <- character(length(code_lu)-1)
  for (i in 1:(length(code_lu)-1)) {
    next_codes[i] <- dir_pad_dirs_mat[ code_lu[i], code_lu[i+1] ]
  }
  next_codes
}

unique_dirs |> str_split("") |>
  map(get_next_dir_pad_code)

# ok, these are the we accumulate. Map this to a matrix of directions
dir_map <- tibble(dir_ind = seq_along(unique_dirs), dirs = unique_dirs)

next_dir_map <- dir_map |>
  mutate(split = str_split(dirs, "")) |>
  mutate(next_dir = map(split, get_next_dir_pad_code)) |>
  unnest(next_dir) |>
  left_join(dir_map, join_by(next_dir == dirs)) |>
  select(dir_ind.x, dir_ind.y) |>
  as.matrix()

next_dirs <- matrix(0, nrow=nrow(dir_map), ncol=nrow(dir_map))
next_dirs[next_dir_map] <- 1

# ok, so next_dirs now is a transistion matrix from dirpad k to dirpad k+1
next_dirs

# notice nothing maps to columns 11, 14 or 15 (<<, >> and v<). Latter is
# because we don't need to go from ^ to < ever, as we've defined down left
# as left then down everywhere else, so will not hit this.
# we could thus simplify the matrix to 12x12 if we want.

# so strategy is going to be:
# 1. Find all possible routes in the numeric pad for the code (not too many,
#    some of them can probably be eliminated, but why bother?)
# 2. Map these to the first directional pad, as there'll be moves that
#    we don't have in the our "unique_dirs".
# 3. We now have our unique_dirs so can just count up the number of each
#    move type and then multiply up by the transistion matrix

get_code_length <- function(code, robots=2) {
  cat("working on code", code, "\n")
  code <- code |> str_split("") |> unlist()

  code_lu <- tibble(value=c("A", code)) |> left_join(num_long) |> pull(vertex)

  # grab all combinations of this code
  next_codes <- ""
  for (i in 1:(length(code_lu)-1)) {
    curr <- num_pad_dirs[[code_lu[i]]][[code_lu[i+1]]]
    next_codes <- paste0(rep(next_codes, each=length(curr)), curr, 'A')
  }
  next_codes <- next_codes |> str_split("", n=Inf)

  cat("we have ", length(next_codes), "for the second robot\n")

  # get the numpad code for the first robot (this is not
  # as optimised as we have more directions inbetween the A presses)

  get_dir_pad_for_num_pad_code <- function(code) {
    code <- c("A", code, "A")

    dir_order <- tibble(order=seq_along(dir_pad_dirs_order),
                        value=dir_pad_dirs_order)

    code_lu <- tibble(value=code) |>
      left_join(dir_order) |> pull(order)

    next_codes <- character(length(code_lu)-1)
    for (i in 1:(length(code_lu)-1)) {
      next_codes[i] <- dir_pad_dirs_mat[ code_lu[i], code_lu[i+1] ]
    }
    next_codes
  }

  numpad1 <- map(next_codes, get_dir_pad_for_num_pad_code)

  # ok, now count these
  count_num_pad_dirs <- function(code, robots) {
    dir_counts <- dir_map |> left_join(tibble(dirs=code) |> count(dirs)) |>
      replace_na(list(n=0))

    # ok, with our dir_counts, transform by our matrix
    for (i in 2:robots) {
      dir_counts <- dir_counts |>
        mutate(n = (n %*% next_dirs) |> as.vector())
    }
    dir_counts |>
      mutate(length = (nchar(dirs)+1)*n) |>
      summarise(len = sum(length) - 1) |>
      pull(len)
  }
  map_dbl(numpad1, count_num_pad_dirs, robots=robots) |>
    min()
}

# part 1:
test_codes <- c("029A", "980A", "179A", "456A", "379A")
p2_test <- map_dbl(test_codes, get_code_length)
p1_test <- test_codes |> str_sub(1,3) |> as.numeric()
sum(p1_test * p2_test) # 126384, correct!

codes <- read_lines("day21/input.txt")
p2 <- map_dbl(codes, get_code_length)
p1 <- codes |> str_sub(1,3) |> as.numeric()
sum(p1*p2) # yes, this is correct.

# part 2
p2_part2 <- map_dbl(codes, get_code_length, robots=25)
sum(p1*p2_part2) # yes, this is correct
