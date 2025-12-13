library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines("2023/day14/input.txt") |>
  str_split_fixed('', n=Inf)

# tilt the map
tilt <- function(col) {
  data.frame(gap=cumsum(col == "#"), col) |>
    group_by(gap) |>
    mutate(moved = c("#", rep("O", sum(col == "O")), rep(".", n()-sum(col == "O")-1))) |>
    pull(moved)
}

input <- rbind(rep('#', nrow(input)), input)
tilted <- apply(input, 2, tilt)

load <- rowSums(tilted == "O") * (nrow(tilted):1)
sum(load)

# part 2: looks like a fun time... Need to repeat this 4 times over 1000000000 times...
# presumably this just operates on vectors though, alternating rolls etc.

input <- "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...." |>
  str_split("\n") |>
  unlist() |>
  str_split_fixed('', n=Inf)

input <- rbind(rep('#', nrow(input)), input, rep('#', nrow(input)))
input <- cbind(rep('#', nrow(input)), input, rep('#', nrow(input)))

cycle <- function(m) {
  cw <- function(m) {
    t(m[nrow(m):1,])
  }
  ccw <- function(m) {
    t(m[,ncol(m):1])
  }
  t1 <- apply(m, 2, tilt) # tilts north
  t2 <- apply(cw(t1), 2, tilt) # tilts west
  t3 <- apply(cw(t2), 2, tilt)
  t4 <- apply(cw(t3), 2, tilt)
  cw(t4)
}

load <- function(m) {
  m <- m[-c(1, nrow(m)),-c(1, ncol(m))]
  sum(rowSums(m == "O") * (nrow(m):1))
}

loads <- accumulate(1:101, \(x, y) cycle(x), .init = input) |>
  map_dbl(load)
table(loads[-c(1:4)]) # period of 7 by the looks?
poss <- (13+1:7)
loads[poss[which(poss %% 7 == 1000000000 %% 7)]+1]

# do the same thing with our grid. Have to initially run
# a bunch longer then look at our repeats
thou <- accumulate(1:200, \(x, y) cycle(x), .init = input)

# periods in the real input seem at least 100 though...
ans <- map_dbl(thou, load)
table(ans[-c(1:100)]) # period of 17 by the looks

poss <- 180 + 1:17
ans[poss[which(poss %% 17 == 1000000000 %% 17)] + 1]
