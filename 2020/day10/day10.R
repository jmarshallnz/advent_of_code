library(tidyverse)

options(digits = 22,
        pillar.max_dec_width = 22)

input <- read.table("2020/day10/input.txt", header=FALSE) |> set_names(nm='jolt')

jolts <- input |> bind_rows(tibble(jolt = c(0, max(input$jolt)+3))) |>
  arrange(jolt) |> rowid_to_column('adapter')

jolts |>
  mutate(diff = jolt - lag(jolt)) |>
  filter(!is.na(diff)) |>
  count(diff) |>
  summarise(prod(n))

# Now count all possible arrangements. This is the number of paths in an acylic graph
# as the graph is acylic, it should be a straightforward igraph problem, except
# igraph doesn't have a count_paths() function, instead wanting to return them all

# so we'll instead need to run it ourselves with a simple dfs I guess?
# figure out our nb list: nb for jolt K are all with input K, K+1, K+2, K+3

nb <- jolts |>
  cross_join(jolts) |>
  filter(jolt.y > jolt.x, jolt.y <= jolt.x+3) |>
  select(adapter.x, adapter.y) |>
  nest(nb = adapter.y) |>
  pull(nb) |> map(\(x) x |> pull(adapter.y))

count_paths <- function(node, dest) {
  if (node == dest) {
    return(1) # have a path
  }
  # visit all our neighbours
#  cat("visiting node", node, "\n")
  if (counts[node] > 0)
    return(counts[node]) # already done

  # check all our neighbours
  count <- 0
  for (n in nb[[node]]) {
    # visit it
    count <- count + count_paths(n, dest)
  }
  cat("done node", node, "\n")
  counts[node] <<- count # save it
  return(count)
}

counts <- rep(FALSE, length(nb)+1) # last node as well
count_paths(1, 104)
