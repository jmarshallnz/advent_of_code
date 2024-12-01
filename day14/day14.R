library(tidyverse)

input <- read.table(text="p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3", sep=" ")

input <- read.table("day14/input.txt", sep=" ")
d <- input |>
  extract(V1, into=c("px","py"), regex="p=([0-9]+),([0-9]+)", convert=TRUE) |>
  extract(V2, into=c("vx","vy"), regex="v=([-0-9]+),([-0-9]+)", convert=TRUE)

width <- 101
height <- 103
#width <- 11
#height <- 7

d |>
  mutate(px = (px + 100*vx) %% width,
         py = (py + 100*vy) %% height) |>
  mutate(quad1 = case_when(px < (width-1)/2 ~ 1,
                           px > (width-1)/2 ~ 2,
                           TRUE ~ NA),
         quad2 = case_when(py < (height-1)/2 ~ 1,
                           py > (height-1)/2 ~ 2,
                           TRUE ~ NA)) |>
  group_by(quad1, quad2) |> summarise(n=n(), .groups = 'drop') |>
  na.omit() |>
  summarise(prod(n))

# Part 2 seems fun - christmas trees! Ofcourse we don't know what it looks like, but
# there will be some structure of some kind, which will likely (but not necessarily?)
# show up in summary statistics.

# If we are very lucky, it might show up in statistics separately in x and y.

# one way to detect structure is just see if there's a difference in standard deviation?
d |> rowid_to_column() |> cross_join(tibble(i = 1:width)) |>
  mutate(px = (px + i*vx) %% width) |>
  group_by(i) |>
  summarise(heavy = sd(px)) |>
  arrange(heavy) # i=97 seems odd

d |> rowid_to_column() |> cross_join(tibble(i = 1:height)) |>
  mutate(py = (py + i*vy) %% height) |>
  group_by(i) |>
  summarise(sd = sd(py)) |>
  arrange(sd) # i=50

# we need these to coincide with each other. Easiest way is to just see when they align?
expand_grid(i=1:width, j=1:height) |>
  mutate(foo = 97+i*width, bar = 50+j*height) |>
  filter(foo == bar) # value is 7672

# let's see:
value <- 7672
d |> mutate(px = (px + value*vx) %% width,
            py = (py + value*vy) %% height) |>
  ggplot() +
  geom_point(aes(x=px, y=py)) #:)
