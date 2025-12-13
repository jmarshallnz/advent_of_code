library(tidyverse)
library(igraph)

options(digits = 22,
        pillar.max_dec_width = 22)

raw <- read.csv("2025/day08/input.txt", header=FALSE)

dists <- dist(raw) |>
  as.matrix() |>
  as.data.frame() |>
  tibble::rowid_to_column() |>
  pivot_longer(-rowid, names_to='colid', values_to = 'dist') |>
  mutate(colid = as.numeric(colid)) |>
  filter(rowid < colid)

closest <- dists |>
  slice_min(dist, n=1000)

g <- closest |> select(rowid, colid) |> as.matrix() |>
  graph_from_edgelist(directed=FALSE)

tibble(sizes = components(g)$csize) |>
  slice_max(sizes, n=3, with_ties=FALSE) |>
  summarise(prod(sizes))

# part 2:
# ok, now we want to know the smallest number of connections so we get a single component
closest <- dists |>
  arrange(dist)

# ideally we do a binary search or thereabouts based on number of components in the graph.
# Not sure if there's a graph theory solution to this. The "minimum spanning tree" type
# ideas won't work as we require using the 'n' shortest edges rather than finding
# the optimal total length (which might involve using a longer edge)

num_components <- function(n) {
  closest <- dists |>
    slice_min(dist, n=n, with_ties=FALSE)
  g <- closest |> select(rowid, colid) |> as.matrix() |>
    graph_from_edgelist(directed=FALSE)
  return(components(g)$no)
}

# by hand binsearch as the gtools::binsearch() function doesn't allow a flat portion.
# TBH it's fast enough that we could just iterate linearly anyway
num_components(10000)
num_components(5000)
num_components(7500)
num_components(8000)
num_components(7750)
num_components(7675)
num_components(7730)
num_components(7700)
num_components(7715)
num_components(7723)
num_components(7719)
num_components(7721)
num_components(7720) # ok, 7721

dists |>
  slice_min(dist, n=7721, with_ties=FALSE) |>
  slice_max(dist, n=1) |>
  select(rowid, colid) |>
  as.numeric() -> rows

prod(raw[rows,]$V1)

# ALTERNATIVE FAST SOLUTION: this is just single linkage hierarchical clustering
# so we can just do it immediately using `hclust`.
# part 1 is cutting the tree at the 1000th distance, part 2 is finding
# the distance at the top of the tree.

raw <- read.csv('2025/day08/input.txt', header=FALSE)
d <- dist(raw)
h <- hclust(d, method='single')
prod(sort(table(cutree(h, h=sort(d)[1000])), decreasing = TRUE)[1:3])
prod(raw[which(as.matrix(d) == max(h$height), arr.ind=TRUE)[1,],]$V1)
