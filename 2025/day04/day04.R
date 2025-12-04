library(tidyverse)

input <- readLines("2025/day04/input.txt") |>
  str_split_fixed('', n=Inf) |>
  as.data.frame() |>
  tibble::rowid_to_column('row') |>
  pivot_longer(-row, names_to='col', values_to='entry') |>
  extract(col, into='col', regex="([[:digit:]]+)", convert=TRUE)

input_shift <- input |>
  mutate(row_before = row-1, row_after=row+1,
         col_before = col-1, col_after=col+1)

input |> left_join(input_shift, join_by(between(row, row_before, row_after),
                                        between(col, col_before, col_after))) |>
  filter(entry.x == "@", entry.y == "@") |>
  count(row.x, col.x) |>
  filter(n <= 4) |>
  nrow()

grid <- input |> left_join(input_shift, join_by(between(row, row_before, row_after),
                                                between(col, col_before, col_after)))

rolls = 0;
# now we just repeat this process over and over. can use anti_join() to drop the rolls
while(TRUE) {
  remove <- grid |>
    filter(entry.x == "@", entry.y == "@") |>
    count(row.x, col.x) |>
    filter(n <= 4)

  if (nrow(remove) == 0) {
    break
  }

  grid <- grid |> anti_join(remove) |>
    anti_join(remove, join_by(row.y==row.x,
                              col.y==col.x)) # removes the ones we already have taken care of

  rolls <- rolls + nrow(remove)
}
rolls

# graph solution: get the graph which is just the @'s joined up if they're adjacent
library(igraph)
vertices <- input |>
  filter(entry == "@") |>
  tibble::rowid_to_column('vid')

graph <- vertices |>
  left_join(vertices |> mutate(row_before = row-1, row_after=row+1,
                               col_before = col-1, col_after=col+1),
            join_by(between(row, row_before, row_after),
                    between(col, col_before, col_after))) |>
  select(vid.x, vid.y) |> as.matrix() |>
  graph_from_edgelist()

# part 1:
sum(degree(graph, mode='out') <= 4)

# part 2:
reduced <- graph
while(TRUE) {
  reachable <- degree(reduced, mode='out') <= 4
  removed   <- V(reduced)[reachable]
  if (length(removed) == 0)
    break
  reduced <- reduced |> delete_vertices(removed)
}
length(V(graph)) - length(V(reduced))
