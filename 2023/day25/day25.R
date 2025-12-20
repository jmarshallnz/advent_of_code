library(tidyverse)
library(igraph)

input <- "jqt: rhn xhk nvd
rsh: frs pzl lsr
xhk: hfx
cmg: qnr nvd lhk bvb
rhn: xhk bvb hfx
bvb: xhk hfx
pzl: lsr hfx nvd
qnr: nvd
ntq: jqt hfx bvb xhk
nvd: lhk
lsr: lhk
rzs: qnr cmg lsr rsh
frs: qnr lhk lsr" |>
  str_split("\n") |> unlist() |>
  str_split(':? ')

input <- readLines('2023/day25/input.txt') |>
  str_split(':? ')

# ok, so links are
edges <- input |>
  map(\(x) { cbind(rep(x[1], length(x)-1), x[-1]) }) |>
  do.call(rbind, args=_)

g <- graph_from_edgelist(edges, directed=FALSE)
# find a cut of size 3

plot(g)

cut <- min_cut(g, value.only=FALSE)
length(cut$partition1)*length(cut$partition2)
