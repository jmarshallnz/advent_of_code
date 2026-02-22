library(tidyverse)
library(collections)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines('2020/day20/input.txt')

tiles <- tibble(input=input, gap = cumsum(str_detect(input, "Tile"))) |>
  group_by(gap) |>
  mutate(tile = input[str_detect(input, "Tile")]) |>
  extract(tile, into='tile', regex="([[:digit:]]+)", convert=TRUE) |>
  filter(input != "") |>
  filter(!str_detect(input, "Tile")) |>
  ungroup() |> select(-gap) |>
  nest(data=input) |>
  mutate(map = map(data, \(x) x |> pull(input) |> str_split_fixed(pattern='', n=Inf))) |>
  select(tile, map)

# OK, now we want to find which borders line up exactly with which other borders
# there could be multiple ofc for each one.

# we know they have to fit into 12x12 grid. Let's start by extracting borders.
# We know there are 1024 possible borders, and we have 4 per tile PLUS 2 orientations
# per border, so 8*144 borders = 1152. So we know there's going to be repeats.
# let's count the number of them for a start?

borders <- function(tile) {
  border <- list(tile[1,], tile[nrow(tile),], tile[,1], tile[,ncol(tile)])
  c(border, map(border, rev))
}

edges <- tiles |>
  mutate(borders = map(map, borders)) |>
  unnest(borders) |>
  add_count(borders) |>
  filter(n==1)

# part 1 can be solved without even assembling:

edges |>
  count(tile) |>
  filter(n==4) |>
  summarise(prod(tile))

# Part 2: Hah, now we actually have to assemble the thing LOL...

# We know one corner tile and can work from there. We don't know
# which the other ones are but can flood-fill I think?
top_left_id <- edges |>
  count(tile) |>
  filter(n==4) |>
  pull(tile) |> first()

tl_borders <- edges |> filter(tile == top_left_id) |>
  pull(borders)

flip_tile <- function(tile) {
  flips <- list(tile, tile[nrow(tile):1,], tile[,ncol(tile):1], tile[nrow(tile):1, ncol(tile):1])
  c(flips, map(flips, t))
}

top_left_tile <- (tiles |> filter(tile == top_left_id) |> pull(map))[[1]]
flips <- flip_tile(top_left_tile)
top  <- map(flips, \(x) x[1,])
left <- map(flips, \(x) x[,1])

# we need both the top and the left to be in our tl_borders
wch <- map2_lgl(top, left, \(x, y) any(map_lgl(tl_borders, \(z) all(z == x))) & any(map_lgl(tl_borders, \(z) all(z == y))))

map <- matrix(NA_character_, nrow=nrow(top_left_tile)*12, ncol=ncol(top_left_tile)*12)

all_flipped <- tiles |> mutate(flipped = map(map, flip_tile)) |>
  unnest(flipped) |>
  mutate(top = map(flipped, \(x) x[1,]),
         left = map(flipped, \(x) x[,1]),
         bottom = map(flipped, \(x) x[nrow(x),]),
         right = map(flipped, \(x) x[,ncol(x)]))

# OK, now flood fill our from the top left
border_left <- left[wch] |> first()
border_top  <- top[wch] |> first()
for (y in 0:11) {
  # find the entry that has the correct border_left
  maplist <- all_flipped |> semi_join(tibble(left=list(border_left)))
  if (nrow(maplist) == 1) {
    all_flipped <- all_flipped |> filter(tile != maplist$tile)
    map[1:10,y*10+1:10] <- maplist$flipped[[1]]
    border_left = maplist$right[[1]]
    border_top = maplist$bottom[[1]]
  } else if (nrow(maplist) > 1) {
    cat("too many hits?!?\n")
  } else {
    cat("no hits??!?\n")
  }
  for (x in 1:11) {
    # find the entry that has the correct border_top
    maplist <- all_flipped |> semi_join(tibble(top=list(border_top)))
    if (nrow(maplist) == 1) {
      all_flipped <- all_flipped |> filter(tile != maplist$tile)
      map[x*10+1:10,y*10+1:10] <- maplist$flipped[[1]]
      border_top = maplist$bottom[[1]]
    } else if (nrow(maplist) > 1) {
      cat("too many hits?!?\n")
    } else {
      cat("no hits??!?\n")
    }
  }
}

# check:
all(map[,seq(10,110,by=10)] == map[,seq(10,110,by=10)+1])
all(map[seq(10,110,by=10),] == map[seq(10,110,by=10)+1,])

# OK, remove all borders
keep <- c(FALSE, rep(TRUE, 8), FALSE)
final_map <- map[keep,keep]

# OK, now flip our map 8 times and look for a monster
all_maps <- flip_tile(final_map) |>
  map(\(x) apply(x, 2, \(y) y == "#"))

count_monsters <- function(map) {
  monster <- '                  #
#    ##    ##    ###
 #  #  #  #  #  #   ' |> str_split('\n') |> unlist() |>
    str_split_fixed('', n=Inf) |>
    apply(2, \(x) x == "#")

  # OK iterate over our map checking for sea monsters
  count <- 0
  for (row in seq_len(nrow(map) - nrow(monster) + 1)) {
    for (col in seq_len(ncol(map) - ncol(monster) + 1)) {
      check <- map[row + 1:nrow(monster)-1, col + 1:ncol(monster)-1]
      if (all((check & monster) == monster)) {
        count <- count + 1
      }
    }
  }
  if (count > 0) {
    not_seamonster <- map
    # need to now count the number that AREN'T sea monster
    for (row in seq_len(nrow(map) - nrow(monster) + 1)) {
      for (col in seq_len(ncol(map) - ncol(monster) + 1)) {
        check <- map[row + 1:nrow(monster)-1, col + 1:ncol(monster)-1]
        if (all((check & monster) == monster)) {
          not_seamonster[row + 1:nrow(monster)-1, col + 1:ncol(monster)-1] <-
            not_seamonster[row + 1:nrow(monster)-1, col + 1:ncol(monster)-1] & (!monster)
        }
      }
    }
    return(sum(not_seamonster))
  }
  return(0)
}

map_int(all_maps, count_monsters)
