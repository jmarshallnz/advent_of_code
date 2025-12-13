library(tidyverse)
library(igraph)

options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines("2025/day11/input.txt") |>
  str_split_fixed(':', n=2) |>
  as.data.frame()

g <- input |>
  mutate(nb = str_split(V2, " ")) |>
  unnest(nb) |>
  filter(nb != "") |>
  select(V1, nb) |>
  as.matrix() |>
  graph_from_edgelist()

all_simple_paths(g, from='you', to='out') |>
  length()

# am guessing there's a circuit, or LOTS of paths from svr to dac/fft/out
#all_simple_paths(g, from='svr', to='dac')

distances(g, v='svr', to='dac', mode='out')
distances(g, v='svr', to='fft', mode='out')
distances(g, v='dac', to='fft', mode='out')
distances(g, v='fft', to='dac', mode='out')
# ok, so we have to go svr -> FFT -> DAC -> out
distances(g, v='dac', to='out', mode='out')

all_simple_paths(g, from='dac', to='out') |>
  length() # 3952 from dac -> out

system.time({
 len <- all_simple_paths(g, from='dac', to='fft', mode = 'in', cutoff=15) |>
  length()}) # 3073194
len

# try limiting length...
system.time({
  len <- all_simple_paths(g, from='fft', to='dac', cutoff=15) |>
    length()}) # 3073194
len

# try next length to see if there are longer ones (presumably no loops???)
system.time({
  len <- all_simple_paths(g, from='fft', to='dac', cutoff=16) |>
    length()}) # 9892788 hmmm...
len

# just use the adjacency matrix?
A <- as_adjacency_matrix(g, sparse=TRUE)

# am guessing we actually need to iterate here with some
# memoisation involved. i.e. grab the neighbours, find number of paths
# using them, and iterate in:
library(memoise)
num_paths <- function(from, to) {
  if (from == to) return(1)
  nb <- neighbors(g, from) |> as.integer()
  sum(map_dbl(nb, \(v) num_paths(v, to)))
}
num_paths <- memoise(num_paths)
num_paths(V(g)['svr'], V(g)['out'])
