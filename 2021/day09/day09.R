library(tidyverse)

options(digits = 22,
        pillar.max_dec_width = 22)

map <- readLines('2021/day09/input.txt') |>
  str_split_fixed('', n=Inf) |>
  apply(2, as.numeric)

# righto, grab our neighbours and check for low-points
map_d <- map[c(2:nrow(map), NA),]
map_u <- map[c(NA, 1:(nrow(map)-1)),]
map_l <- map[,c(NA, 1:(ncol(map)-1))]
map_r <- map[,c(2:ncol(map), NA)]

# ok, low is where map is lower than the rest
low_points <- (map < map_d | is.na(map_d)) &
  (map < map_l | is.na(map_l)) &
  (map < map_u | is.na(map_u)) &
  (map < map_r | is.na(map_r))

sum(map[low_points]+1)

# ok, now find basins. For each point, flood fill out
library(collections)

find_basin <- function(low, map) {
  process <- stack(list(low))
  visited <- matrix(FALSE, nrow=nrow(map), ncol=ncol(map))
  visited[low] <- TRUE # done!

  while(process$size()) {
    cur_pos <- process$pop()
#    cat('cur_pos=', cur_pos, '\n')
#    cat('dim=', dim(cur_pos), '\n')
    # get neighbours
    nb <- matrix(c(-1, 0, 1, 0, 0, -1, 0, 1), nrow=4) + matrix(1, nrow=4) %*% cur_pos
    # throw away boundaries
    for (i in 1:nrow(nb)) {
      if (any(nb[i,] < 1 | nb[i,] > 100))
        next
      if (map[nb[i,,drop=FALSE]] == 9) # no basin
        next
      if (map[nb[i,,drop=FALSE]] <= map[cur_pos])
        next # uphill only
      if (!visited[nb[i,,drop=FALSE]]) {
        # add to our process list
        process$push(nb[i,,drop=FALSE])
        visited[nb[i,,drop=FALSE]] <- TRUE
      }
    }
  }
  visited
}

low_pos <- which(low_points, arr.ind = TRUE)
basins <- list()
for (i in 1:nrow(low_pos)) {
  basins[[i]] <- find_basin(low_pos[i,,drop=FALSE], map)
}

map_int(basins, sum) |> sort(decreasing = TRUE) |> head(3) |> prod()

# pretty basin map
basin_map <- matrix(0, nrow(map), ncol(map))
for (i in 1:length(basins)) {
  basin_map[basins[[i]]] <- i
}
image(basin_map)
