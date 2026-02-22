library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

# your classic floodfill problem (8-way)
input <- '5483143223
2745854711
5264556173
6141336146
6357385478
4167524645
2176841721
6882881134
4846848554
5283751526' |>
  str_split('\n') |> unlist()

input <- readLines('2021/day11/input.txt')

map <- input |> str_split_fixed('', n=Inf) |>
  apply(2, as.numeric)

flood_fill <- function(m) {
  # find any > 9
  pos <- which(m > 9, arr.ind=TRUE)
  if (nrow(pos) == 0)
    return(m) # done!
  s <- stack(apply(pos, 1, identity, simplify = FALSE))
  # set these values to 0 to denote visited
  visited <- matrix(FALSE, nrow(m), ncol(m))
  visited[pos] <- TRUE
  while(s$size()) {
    cur = s$pop()
  #  cat('processing', cur, '\n')
    # generate neighbours
    for (x in -1:1) {
      for (y in -1:1) {
        if (cur[1] + x >= 1 && cur[1] + x <= nrow(m) &&
            cur[2] + y >= 1 && cur[2] + y <= ncol(m)) {
          nex_cur <- cur + matrix(c(x, y), nrow=1)
          m[nex_cur] <- m[nex_cur] + 1
          if (m[nex_cur] > 9 && !visited[nex_cur]) {
       #     cat('adding', nex_cur, '\n')
            visited[nex_cur] <- TRUE
            s$push(nex_cur)
          }
        }
      }
    }
  }
  # anything more than 9 we set to 0
  m[m > 9] <- 0
  m
}

flashes <- 0
m <- map
for (d in 1:100) {
  # increase by one
  # floodfill anything > 9
  m <- flood_fill(m+1)
  flashes <- flashes + sum(m == 0)
}

m <- map
for (d in 0:1000) {
  if (d %% 100 == 0)
    cat("iteration i=", d, "\n")
  # increase by one
  # floodfill anything > 9
  m <- flood_fill(m+1)
  if (sum(m) == nrow(m)*ncol(m))
    break
}
d

