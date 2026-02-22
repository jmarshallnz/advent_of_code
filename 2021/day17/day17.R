library(tidyverse)

# If we're shooting down then we can't be smaller than -141.
input <- readLines('2021/day17/input.txt')
input <- 'target area: x=20..30, y=-10..-5'

# extract our target area
target_x1 <- str_extract(input, 'x=([0-9]+)\\.\\.([0-9]+)', group=1) |> as.integer()
target_x2 <- str_extract(input, 'x=([0-9]+)\\.\\.([0-9]+)', group=2) |> as.integer()
target_y1 <- str_extract(input, 'y=([-0-9]+)\\.\\.([-0-9]+)', group=1) |> as.integer()
target_y2 <- str_extract(input, 'y=([-0-9]+)\\.\\.([-0-9]+)', group=2) |> as.integer()

# ok, now fire with a given velocity, following our path accordingly
trajectory <- function(dx, dy, floor_y) {
  pos_x <- 0; pos_y <- 0
  traj <- list(data.frame(x = pos_x, y = pos_y))
  # iterate
  while(pos_y > floor_y) {
    pos_x <- pos_x + dx # velocity
    pos_y <- pos_y + dy # velocity
    dx <- dx - sign(dx) # drag
    dy <- dy - 1 # gravity
    # append to our list
    traj[[length(traj) + 1]] <- data.frame(x=pos_x, y=pos_y)
  }
  return(bind_rows(traj))
}

hits_target(7, 2)
hits_target(6, 3)
hits_target(9, 0)
hits_target(17, -4)

# find the one with the maximum Y in the trajectory that hits.
# I guess brute forcing is probably doable?
# Our target is below 0, so the max position will be the max velocity upwards

# If we think about our drag, we can work out how many steps it takes for a given
# dx anyway, right?
# we know after N steps exactly where it will be in X and Y.

# In the example we need to go between 20 and 30 X, so need dx at least 6 and no
# more than 30 (obviously want way less - probably want the minimum so we end up going
# straight down at the end

# If dx < 5 then we never reach it, right?
#
# We'll go HIGHEST if x is as small as possible maybe?
# we need the triangle(x) to be in the range
# Y is then a quadratic, where the velocity to 0 is -dy on the way down
# so we can work out how big Y can be based on -sum_{j=1..k}(dy+j) = -(k*(k+1)/2+dy*k) passing through
# the target. In this case, we know our target is -5:-10 so k has to be

# figure out our min and max ranges
min_dx <- function(x) {
  # minimum is the inverse of the step/triangle function, which is roughly
  test <- floor(sqrt(2*x))
  if (test*(test+1)/2 < x)
    test <- test+1
  test
}

# dy is then as large as possible such that we land inside it
dx <- min_dx(target_x1)
dy <- -(target_y1+1)
trajectory(dx, dy, floor_y=target_y1) |>
  summarise(max(y))

# Part 2: need to find EVERY velocity possible.
# we have limits on dx and dy, and can probably just brute-force it?
# Our limit on dx is that it has to be >15 and <176
# Our limit on dy though? If we're shooting UP then we need it to be less than 140. Any bigger
# and we go over.

hits_target <- function(dx, dy) {
  trajectory(dx, dy, floor=target_y1) |>
    filter(x >= target_x1, x <= target_x2, y >= target_y1, y <= target_y2) |>
    nrow() > 0
}

hits <- expand_grid(x = min_dx(target_x1):target_x2, y = -target_y1:(target_y1-1)) |>
  mutate(traj = map2_lgl(x, y, hits_target))

hits |> filter(traj) |> nrow()

# thinking about a faster way:
ggplot(hits) + aes(x=x, y=y, col=traj) + geom_point()

# If Y is negative then we can figure it out just by backtracking on X?

# Any x,y within the current target will be fine. i.e. if N=1 steps we can
# invert the target to find the range.
# If N=2 we can do the same, right? This constrains X which then constrains Y


