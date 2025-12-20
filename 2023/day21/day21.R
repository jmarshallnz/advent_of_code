library(tidyverse)
library(igraph)
options(digits = 22,
        pillar.max_dec_width = 22)

map <-
  "...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
..........." |> str_split("\n") |> unlist() |>
  str_split_fixed('', n=Inf)

map <- readLines("2023/day21/input.txt") |>
  str_split_fixed('', n=Inf)

# For part 1 need to know how many he visits
# after exactly 64 steps. Thus, need a path from
# S to any other location that is exactly 64
# long.
# as we can repeat vertices, this is in actual
# practice any path of length 0..2..64 long
# as we can always walk back and forth.
# can we get an odd-length circuit?
# I don't think so, as moves can only be
# horizontal or vertical, and any move
# moves to a different parity square.
# So simple thing: Find all '.' within
# distance "0, 2, ... 64" of S, noting
# that some squares (#) are inaccessible.

# probably easiest with igraph. We could use
# the 64 limit to reduce the search space a
# bit (he can't go too far) but it's likely
# not worth the hassle
to_graph <- function(vertices) {
  vertices_shift <- vertices |>
    mutate(row_before = row-1, row_after=row+1,
           col_before = col-1, col_after=col+1)

  vertices |> left_join(vertices_shift,
                        join_by(between(row, row_before, row_after),
                                between(col, col_before, col_after))) |>
    filter((row.x - row.y)*(col.x - col.y) == 0) |>
    select(vertex.x, vertex.y) |>
    as.matrix() |>
    graph_from_edgelist()
}

to_vertices <- function(map) {
  map |> as.data.frame() |>
    tibble::rowid_to_column('row') |>
    pivot_longer(-row, names_to='col', values_to='entry') |>
    extract(col, into='col', regex="([[:digit:]]+)", convert=TRUE) |>
    filter(entry != "#") |>
    rowid_to_column('vertex')
}

# ok, now find points the right distance from 'S'
vertices <- to_vertices(map)
start_id <- vertices |> filter(entry == 'S') |> pull(vertex)
graph <- to_graph(vertices)

vertices |>
  mutate(from_s = distances(graph, v = start_id) |> as.numeric()) |>
  filter(from_s %in% seq(0,64,by=2)) |>
  nrow()

# part 2: This one is fun! We have an infinite graph and need
# to again count the number that are exactly 26501365 steps
# away.

# if we go far enough away from the origin, then our optimal way
# will be from S to one of the corners, then from that corner
# to another (via the outside endges) and then from the corner
# to the destination.

# I think there's 8 possible cases depending on our direction
# mx, my (think the diagonals plus the maps directly horizontal
# or vertical from our start point). Reason is the diagonals
# will be "closer" as from closest corner to closest corner
# the distance is only 2 (plus modulo map dim less 2), whereas for
# straight lines it's 1 (plus modulo map dim less 1).
# possibilities are:

# mx = 0, my >> 0
# mx = 0, my << 0
# my = 0, mx >> 0
# my = 0, mx << 0
# mx > 0, my > 0
# mx > 0, my < 0
# mx < 0, my > 0
# mx < 0, my > 0

directions <- expand_grid(mx = c(-1,0, 1), my=c(-1, 0, 1)) |>
  filter(mx != 0 | my != 0)

corners <- expand_grid(row=c(1,nrow(map)), col=c(1,nrow(map)))

# ok now each direction is associated with one or more corners being
# optimal
middle <- (nrow(map)+1)/2
scorner <- directions |>
  cross_join(corners) |>
  mutate(drow = row - middle, dcol = col - middle) |>
  filter(my*drow >= 0, mx*dcol >= 0) |>
  select(mx, my, srow=row, scol=col)

dcorner <- directions |>
  cross_join(corners) |>
  mutate(drow = row - middle, dcol = col - middle) |>
  filter(my*drow <= 0, mx*dcol <= 0) |>
  select(drow=row, dcol=col)

dir_corners <- scorner |> bind_cols(dcorner) |> left_join(vertices, by=join_by(srow==row, scol==col)) |>
  group_by(mx, my) |>
  mutate(dist_to_s = distances(graph, v=start_id, to=vertex) |> as.numeric(), .keep='unused') |>
  select(-entry)

# ok, now we need the distance to from every point in the grid to each drow/dcol
dist <- dir_corners |> left_join(vertices |> select(-entry), by=join_by(drow==row, dcol==col)) |>
  rename(dvertex = vertex) |>
  cross_join(vertices) |>
  group_by(mx, my, drow, dcol) |>
  mutate(dist_to_d = distances(graph, v=first(dvertex), to=vertex) |> as.numeric()) |>
  group_by(mx, my, vertex) |>
  summarise(shortest = min(dist_to_s + dist_to_d))

#-1, 0 go to drow,dcol, go across 11*(dia-1)+1, go to s.
#1, 0 go to drow,dcol, go across 11*(dia-1)+1, go to s.
#-1, -1 go to drow, dcol, go 11*(dia-2) blocks + 2, go to s.

# for our big map, it's much easier as there's paths directly from S to the middle
# so we can drop the dist_to_s entirely (it's 65 in every case) and just use
# dist_to_d where d is the closest middle edge.

corners <- tribble(~row, ~col,
                   1, 1,
                   1, nrow(map),
                   nrow(map), 1,
                   nrow(map), nrow(map),
                   1, middle,
                   nrow(map), middle,
                   middle, 1,
                   middle, nrow(map))

scorner <- directions |>
  cross_join(corners) |>
  mutate(drow = row - middle, dcol = col - middle) |>
  filter(sign(mx) == sign(drow) & sign(my) == sign(dcol)) |>
  select(mx, my, scol=col, srow=row) |>
  mutate(dcol = nrow(map)-scol+1, drow = nrow(map)-srow+1)

dir_corners <- scorner |> left_join(vertices, by=join_by(srow==row, scol==col)) |>
  group_by(mx, my) |>
  mutate(dist_to_s = distances(graph, v=start_id, to=vertex) |> as.numeric(), .keep='unused') |>
  select(-entry) # all 65 as expected

dist <- dir_corners |> left_join(vertices |> select(-entry), by=join_by(drow==row, dcol==col)) |>
  rename(dvertex = vertex) |>
  cross_join(vertices) |>
  group_by(mx, my, drow, dcol) |>
  mutate(dist_to_d = distances(graph, v=first(dvertex), to=vertex) |> as.numeric()) |>
  group_by(mx, my, vertex) |>
  summarise(shortest = min(dist_to_s + dist_to_d))

# inside count is the sum of the odds times the sum of the evens based on parity
num_steps <- 26501365
n_even <- floor(floor(num_steps / nrow(map) - 2)/2)
max_dia <- ceiling(num_steps / nrow(map))

parity_counts <- vertices |>
  mutate(d = distances(graph, v=start_id) |> as.numeric()) |>
  mutate(parity = d %% 2) |>
  count(parity) # 7623 evens, 7558 odds.
# at 0 we want the ODDS

inside_ring_count <-
  tibble(dia = 1:(max_dia-2)) |>
  mutate(parity = (dia-1) %% 2) |>
  mutate(total = (dia-1)*nrow(map)+65) |>
  left_join(block_parity) |>
  mutate(circ = 4*dia) |>
  group_by(dia) |>
  summarise(ans=sum(circ*n))

middle_block_count <-
  block_parity |> filter(parity == 1) |> pull(n)

# final rings
outside_ring_count <-
  tibble(dia = (max_dia-1):max_dia) |>
  cross_join(dist) |>
  left_join(vertices) |>
  group_by(mx, my, dia) |>
  mutate(total = shortest + if_else(mx*my == 0, (dia-1)*nrow(map)+1, (dia-2)*nrow(map) + 2)) |>
  arrange(total) |> # smallest is 26501366?
  summarise(ok = sum(total <= num_steps & total %% 2 == num_steps %% 2)) |>
  ungroup() |>
  mutate(circ = ifelse(mx*my == 0, 1, dia-1)) |>
  group_by(dia) |>
  summarise(ans=sum(circ*ok))

#    /^^\
#  /*][][*\
# <][][][][> dist to outside: 131*2+65
#  \*][][*/
#    \VV/

# last ring is ring 2 is not entirely inside.
# and ring 3 is only inside a tiny bit

# ok, now add them together
sum(middle_block_count) + sum(inside_ring_count |> pull(ans)) + sum(outside_ring_count |> pull(ans))

# fuck yeah!

# Plot the damn thing!
im <- matrix(0, nrow(map), ncol(map))
im[vertices |> select(row, col) |> as.matrix()] <- 1
image(im)

# we can see it's a diamond which will always be inside
# and some triangles, of which some will be inside and
# some won't.

# so split it up:
diamond <- vertices |>
  filter(row+col > 66) |>
  filter((132-row)+(132-col) > 66) |>
  filter((132-row)+col > 66) |>
  filter(row+(132-col) > 66) |>
  select(-vertex) |>
  tibble::rowid_to_column('vertex')

start_di <- diamond |>
  filter(entry == 'S') |>
  pull(vertex)
digraph <- to_graph(diamond)
diamond |>
  mutate(from_s = distances(digraph, v = start_di) |> as.numeric()) |>
  filter(is.finite(from_s)) |>
  summarise(max(from_s)) # some Inf, but ignoring them? # ok, we can get anywhere in 65...

im <- matrix(0, nrow(map), ncol(map))
im[diamond |> select(row, col) |> as.matrix()] <- 1
image(im)

# triangles
triangles <- vertices |>
  select(-vertex) |>
  anti_join(diamond) |>
  rowid_to_column('vertex')

im <- matrix(0, nrow(map), ncol(map))
im[triangles |> select(row, col) |> as.matrix()] <- 1
image(im)

parity <- vertices |>
  mutate(from_s = distances(graph, v = start_id) |> as.numeric()) |>
  mutate(parity = from_s %% 2) |>
  select(row, col, parity)

tri_parity <- triangles |>
  left_join(parity) |>
  count(parity)

dia_parity <- diamond |>
  left_join(parity) |>
  filter(is.finite(parity)) |>
  count(parity)

block_parity <- vertices |>
  left_join(parity) |>
  filter(is.finite(parity)) |>
  count(parity)

num_steps <- 26501365
max_blocks <- floor(num_steps / nrow(map))
max_blocks

inside_blocks = max_blocks-1

n_even <- (max_blocks-1)/2
# 1..202299 is what we need
# some of the odds to 202299: x = 2n-1
# 1 + 3 + 5 = sum(2n-1) = 2*sum(n) - n(?) = ((x+1)/2)^2
# some of the evens to 202298:
# 2 + 4 + ... = 1 + 3 + 5 - n = n^2-n
inside_count <- tribble(~parity, ~count,
                        1, 4*(((inside_blocks-1)/2)^2+(inside_blocks-1)/2),
                        0, 4*((inside_blocks+1)/2)^2) |>
  left_join(block_parity) |>
  summarise(inside = sum(count*n) + block_parity |> filter(parity == 1) |> pull(n)) |>
  pull(inside)

# the final radius is then the diamond plus two triangles on left
# above then diamond plus lower left triangle (or diamond plus upper left triangle etc)
# i.e. 4 diamonds plus 3 each of the triangles per 202300 diagonal,
# and 4 diamonds plus 2 each of the triangles per 202300 straight.

outer_ring_dia <- inside_blocks+1
diag_blocks <- tri_parity |> mutate(type='triangle', rep=3) |> bind_rows(dia_parity |> mutate(type='diamond', rep=4)) |>
  mutate(circ = outer_ring_dia-1)
strt_blocks <- tri_parity |> mutate(type='triangle', rep=2) |> bind_rows(dia_parity |> mutate(type='diamond', rep=4)) |>
  mutate(circ = 1)

outer_ring1 <- bind_rows(diag_blocks, strt_blocks) |>
  filter(parity == (outer_ring_dia-1) %% 2) |>
  summarise(ans=sum(rep*circ*n)) |>
  pull(ans)

outside_ring_count
outer_ring_dia <- inside_blocks+2
diag_blocks <- tri_parity |> mutate(type='triangle', rep=1) |>
  mutate(circ = outer_ring_dia-1)
outer_ring2 <- diag_blocks |>
  filter(parity == (outer_ring_dia-1) %% 2) |>
  summarise(ans=sum(rep*circ*n)) |>
  pull(ans)

# then add them all up and we should have our answer
outer_ring1+outer_ring2 +inside_count
# 621289922886149
