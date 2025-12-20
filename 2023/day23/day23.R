library(tidyverse)
library(igraph)

map <- "#.#####################
#.......#########...###
#######.#########.#.###
###.....#.>.>.###.#.###
###v#####.#v#.###.#.###
###.>...#.#.#.....#...#
###v###.#.#.#########.#
###...#.#.#.......#...#
#####.#.#.#######.#.###
#.....#.#.#.......#...#
#.#####.#.#.#########v#
#.#...#...#...###...>.#
#.#.#v#######v###.###v#
#...#.>.#...>.>.#.###.#
#####v#.#.###v#.#.###.#
#.....#...#...#.#.#...#
#.#########.###.#.#.###
#...###...#...#...#.###
###.###.#.###v#####v###
#...#...#.#.>.>.#.>.###
#.###.###.#.###.#.#v###
#.....###...###...#...#
#####################.#" |>
  str_split("\n") |> unlist() |>
  str_split_fixed('', n=Inf)

map <- readLines('2023/day23/input.txt') |>
  str_split_fixed('', n=Inf)

# ok, now just iterate
start <- data.frame(row=1, col=which(map[1,] == "."))
end <- data.frame(row=nrow(map), col=which(map[nrow(map),] == "."))

# measure our path length

get_neighbours <- function(pos, prev_pos) {
#  downhill <- matrix(c(0, 1, 0, -1, 1, 0, -1, 0), byrow=TRUE, ncol=2, dimnames = list(c('>','<','v','^'), NULL))
#  if (map[pos] %in% rownames(downhill)) {
#    nb_dir <- downhill[map[pos],,drop=FALSE]
#  } else {
    nb_dir <- matrix(c(0, 1, 0, -1, 1, 0, -1, 0), byrow=TRUE, ncol=2)
#  }
  nb <- matrix(1, nrow=nrow(nb_dir)) %*% pos + nb_dir
  nb <- nb[nb[,1] >= 1 & nb[,1] <= nrow(map) &
           nb[,2] >= 1 & nb[,2] <= ncol(map),,drop=FALSE]
  # rule out any we've already been to
  nb <- nb[!prev_pos[nb],,drop=FALSE]
  #nb <- nb[!apply(nb, 1, \(x) all(x == prev_pos)),,drop=FALSE]
  #nb <- nb[!visited[nb],,drop=FALSE]
  # and any that are invalid
  nb <- nb[map[nb] != "#",,drop=FALSE]
  return(nb)
}

longest_path <- function(pos, prev_pos, end) {
  # update our visited flag. We only want to visit each node once?
  cat("checking position", pos, "\n")
  if (all(pos == end)) {
    return(0) # done!
  }
  if (lengths[pos] >= 0) {
    return(lengths[pos]) # not sure if this will be best yet?
  }
  # else find our neighbours
  nb <- get_neighbours(pos, prev_pos)
  if (nrow(nb) == 0) {
    # made a wrong turn
    return(-Inf)
  }
 # cat("neighbours are:", nb, "\n")
  # iterate over our neighbours
  visited[pos] <<- visited[pos]+1 # hmm...
  longest <- -Inf
  prev_pos[pos] <- TRUE # we've visited this one
  for (i in 1:nrow(nb)) {
    longest <- max(longest, longest_path(nb[i,,drop=FALSE], prev_pos, end)+1)
  }
#  lengths[pos] <<- longest
  return(longest)
}

pos <- start |> as.matrix()
end <- end |> as.matrix()
prev_pos <- matrix(FALSE, nrow(map), ncol(map))
prev_pos[pos] <- TRUE
lengths  <- matrix(-Inf, nrow(map), ncol(map))
visited  <- matrix(0, nrow(map), ncol(map))
visited[pos] <- 1
lengths[end] <- 0

# we're going to have to recurse perhaps 10k times which is too many
# for the 'normal' R stack.

# I think the answer is to order the nodes recursively maybe?
# this is I think easier?

test <- longest_path(pos, prev_pos, end) # 82 isn't long enough!
# stack overflow FTW...

# ok, part 2 levels it up: we no longer have our direction limitation
# which is going to dramatically increase the complexity
# as it allows us to cycle around.

# let's see if the graph is acyclic
vertices <- map |>
  as.data.frame() |>
  tibble::rowid_to_column('row') |>
  pivot_longer(-row, names_to='col', values_to='entry') |>
  extract(col, into='col', regex="([[:digit:]]+)", convert=TRUE) |>
  filter(entry != "#") |>
  rowid_to_column('vertex')

vertices_shift <- vertices |>
  mutate(row_before = row-1, row_after=row+1,
         col_before = col-1, col_after=col+1)

vertices |> left_join(vertices_shift, join_by(between(row, row_before, row_after),
                                        between(col, col_before, col_after))) |>
  filter(vertex.x < vertex.y) |>
  filter((row.x - row.y)*(col.x-col.y) == 0) |>
#  mutate(downhill = entry.x == '.' | (entry.x=='>' & col.y > col.x) |
#           (entry.x == '<' & col.y < col.x) |
#           (entry.x == 'v' & row.y > row.x) |
#           (entry.x == '^' & row.y < row.x)) |>
#  filter(downhill) |>
  select(vertex.x, vertex.y) |>
  as.matrix() |>
  graph_from_edgelist(directed=FALSE) |>
  set_edge_attr('weight', value=1) -> graph

plot(graph, vertex.size=1)

# ok, see the simple paths?
start_id <- vertices |> filter(row == 1) |> pull(vertex)
end_id <- vertices |> filter(row == max(row)) |> pull(vertex)

# seems way too slow. We can probably study the graph though to
# speed things up?
# it's not acyclic but it's probably almost acyclic?

# iterate through all the 2 degrees and eliminate them, adding weight to the new edge
# based on the combined edges
delete_degree2 <- function(graph) {
  v <- which(degree(graph) == 2)[1]
  # delete v, adding a new edge between it's neighbours
  nb <- neighborhood(graph, nodes=v, mindist=1) |> unlist()
  # find edges
  edges <- E(graph, path = c(nb[1], v, nb[2]))
  new_edge_weight <- sum(edge_attr(graph, 'weight', index=edges))
  add_edges(graph, nb, attr=list(weight=new_edge_weight)) |> delete_vertices(v)
}

num_degree2 <- sum(degree(graph) == 2)
while(num_degree2 > 1) {
  cat("simplifying graph:", num_degree2, "\n")
  graph <- delete_degree2(graph)
  num_degree2 <- sum(degree(graph) == 2)
}

graph |> set_edge_attr('label', value=edge_attr(graph, 'weight')) |> plot(vertex.size=1, vertex.label=NA)

graph |> plot()
# ok, now I think we can basically just explore all this, as long as the code
# is fast enough. e.g. we don't want to be calling into igraph...

# make it faster
neighbours <- neighborhood(graph, node=V(graph), mindist=1) |>
  map(as.numeric)

length <- matrix(0, nrow=37, ncol=37)
length[as_edgelist(graph)] <- edge_attr(graph, 'weight')
length[as_edgelist(graph)[,2:1]] <- edge_attr(graph, 'weight')

find_longest <- function(v, e, curr) {
  # find neighbours
  explored <<- explored+1
  if (v == e)
    return(0)
  nb <- neighbours[[v]]
  nb <- nb[!curr[nb]]
  # ok, iterate over neighbours, finding longest path
  longest <- -Inf
  curr[v] <- TRUE
  for (n in nb) {
    len <- length[n, v]
    longest <- max(longest, find_longest(n, e, curr)+len)
  }
  if (longest > very_longest[v]) {
    cat("at", v, "found one", longest, "long visiting", sum(curr), "nodes hit", explored, "functions\n")
    very_longest[curr] <<- longest
  }
  return(longest)
}

very_longest <- rep(0, 37)
explored <- 0
test = find_longest(v=1, e=37, curr=logical(37))
