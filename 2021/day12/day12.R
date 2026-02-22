library(tidyverse)

edges <- read.table(text='fs-end
he-DX
fs-he
start-DX
pj-DX
end-zg
zg-sl
zg-pj
pj-he
RW-he
fs-DX
pj-RW
zg-RW
start-pj
he-WI
zg-he
pj-fs
start-RW', sep='-', header=FALSE)

edges <- read.table("2021/day12/input.txt", sep='-', header=FALSE)
# create our neighbours
vertices <- unique(c(edges$V1, edges$V2))
nb <- list()
for (i in 1:nrow(edges)) {
  nb[[edges$V1[i]]] <- c(nb[[edges$V1[i]]], edges$V2[i])
  nb[[edges$V2[i]]] <- c(nb[[edges$V2[i]]], edges$V1[i])
}

small_caves <- vertices[str_to_lower(vertices) == vertices]

# ok, now do our path counting
count_paths <- function(node, dest, visited) {
  if (node == dest) {
    return(1) # have a path
  }
  # visit all our neighbours
#  cat("visiting node", node, "\n")
  visited[node] <- visited[node]+1

  # check all our neighbours
  count <- 0
  for (n in nb[[node]]) {
    # is it visited?
    if (str_to_lower(n) != n || visited[n] == 0) {
      # visit it
      count <- count + count_paths(n, dest, visited)
    }
  }
  return(count)
}

visited <- integer(length(vertices))
names(visited) <- vertices
count_paths('start', 'end', visited=visited)

# Part 2: Now we can visit ONE of the lowercase caves twice, and the rest once.
# So we need to change our visited test. If we've visited a lowercase cave twice
# then all others can be once.

# so our visited structure needs to know the visited count? Then the test is
# visited[n] == 0 OR visited[n] == 1 && !any(visited == 2).
# plus, visited[start] can be at most 1, and visited[end] can be at most 1.

count_paths2 <- function(node, dest, visited) {
  if (node == dest) {
    return(1) # have a path
  }
  # visit all our neighbours
  #  cat("visiting node", node, "\n")
  if (str_to_lower(node) == node) {
    visited[node] <- visited[node]+1
  }

  # check all our neighbours
  count <- 0
  for (n in nb[[node]]) {
    # is it visited?
    if (str_to_lower(n) != n || visited[n] == 0 ||
        visited[n] == 1 && n != 'start' && n != 'end' && !any(visited==2)) {
      # visit it
      count <- count + count_paths2(n, dest, visited)
    }
  }
  return(count)
}

visited <- integer(length(vertices))
names(visited) <- vertices
count_paths2('start', 'end', visited=visited)
