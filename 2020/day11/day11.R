library(tidyverse)

options(digits = 22,
        pillar.max_dec_width = 22)

input <- "L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL" |> str_split("\n") |> unlist() |>
  str_split_fixed('', n=Inf)

input <- readLines("2020/day11/input.txt") |>
  str_split_fixed('', n=Inf)

# RULES:
# If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.

# find our neighbours
get_nb_count <- function(map, row, col) {
  count <- 0
  for (ro in -1:1) {
    for (co in -1:1) {
      if (row + ro >= 1 && row + ro <= nrow(map) &&
          col + co >= 1 && col + co <= ncol(map) &&
          !(ro == 0 && co == 0)) {
        count <- count + map[row+ro, col+co]
      }
    }
  }
  return(count)
}

map <- input
for (i in 1:100) {
  # repeat these rules until we're done
  next_map <- map
  occupied <- map == "#"
  # Rule 1: If a seat is empty (L) AND there are no occupied seats adjacent to it, the seat becomes occupied (#)
  empty <- which(map == "L", arr.ind = TRUE)
  empty_change <- map2_lgl(empty[,1], empty[,2], \(x, y) get_nb_count(occupied, x, y) == 0)
  next_map[empty[empty_change,,drop=FALSE]] <- "#"
  # Rule 2: If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
  occup <- which(occupied, arr.ind = TRUE)
  occup_change <- map2_lgl(occup[,1], occup[,2], \(x, y) get_nb_count(occupied, x, y) >= 4)
  next_map[occup[occup_change,,drop=FALSE]] <- "L"
  if (sum(empty_change) + sum(occup_change) == 0)
    break
  cat("iterating...", i, "gives", sum(empty_change), sum(occup_change), "changes\n")
  map <- next_map
}
sum(map == "#")

# Part 2: OK, same thing but different nb definition

# find our neighbours for part 2. Need to search in those 8 directions
get_nb_count2 <- function(map, row, col) {
  count <- 0
  for (ro in -1:1) {
    for (co in -1:1) {
      if (ro == 0 && co == 0) {
        next # skip the zero case
      }
      found <- FALSE
      for (k in 1:nrow(map)) {
        # max possible
        if (row + k*ro < 1 || row + k*ro > nrow(map) ||
            col + k*co < 1 || col + k*co > ncol(map))
          break # we're done
        if (map[row + k*ro, col + k*co] != '.') { # found a seat
   #       cat("found", map[row + k*ro, col + k*co], 'at', row+k*ro, col+k*co, '(', ro, ',', co, ')\n')
          found <- map[row + k*ro, col + k*co] == "#" # found an occupied seat
          break
        }
      }
      count <- count + found
    }
  }
  return(count)
}

map <- input
for (i in 1:100) {
  # repeat these rules until we're done
  next_map <- map
  occupied <- map == "#"
  # Rule 1: If a seat is empty (L) AND there are no occupied seats adjacent to it, the seat becomes occupied (#)
  empty <- which(map == "L", arr.ind = TRUE)
  empty_change <- map2_lgl(empty[,1], empty[,2], \(x, y) get_nb_count2(map, x, y) == 0)
  next_map[empty[empty_change,,drop=FALSE]] <- "#"
  # Rule 2: If a seat is occupied (#) and five or more seats adjacent to it are occupied, the seat becomes empty.
  occup <- which(occupied, arr.ind = TRUE)
  occup_change <- map2_lgl(occup[,1], occup[,2], \(x, y) get_nb_count2(map, x, y) >= 5)
  next_map[occup[occup_change,,drop=FALSE]] <- "L"
  if (sum(empty_change) + sum(occup_change) == 0)
    break
  cat("iterating...", i, "gives", sum(empty_change), sum(occup_change), "changes\n")
  map <- next_map
}
sum(map == "#")

