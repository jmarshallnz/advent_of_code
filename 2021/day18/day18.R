library(tidyverse)
library(collections)

# idea: this is obviously a tree structure.
# EXPLODE means we have to trace back through the tree to the LEFT and RIGHT
# this involves backtracking to parents.

# given a node we need to go node -> parent -> has_left()/right()?

# could we use a doubly-linked tree structure?
# node -> left/right/parent/value, where left is another list?

# with pointers this would be doable 'easily' perhaps we have
# to keep our traverse through the list? e.g. LRRR means to find
# our LEFT we need to go to LRRL (if it exists) else LRL, else LL
# our RIGHT we need to go to back up the R's to the L then R

# otherwise I think actually keeping it linear might make more sense?
# some sort of level and value structure?

homework <- '[[[[[9,8],1],2],3],4]'

# we first need to parse this into a structure. I think the structure
# we want is something like:

tibble(lr = c(1, 2, 3, 4, 5, 6),
       depth = c(5, 5, 4, 3, 2, 1),
       value = c(9, 8, 1, 2, 3, 4))

# to explode we find the first depth == 5 adjacent pair
parse_sf <- function(hw) {
  sf <- hw |> str_split('') |> unlist()
  out <- list()
  depth <- 0
  lr <- 'L'
  pos <- 1
  # find the next character
  i <- 1
  while(i <= length(sf)) {
    ch <- sf[i]
    if (ch == '[') {
      # starting a pair
      depth <- depth+1
      lr <- 'L' # switch to L
      i <- i + 1
    } else if (ch == ']') {
      # ending a pair
      depth <- depth-1; i <- i + 1
    } else if (ch == ',') {
      # switching from L to R
      lr <- 'R'; i <- i + 1
    }
    else { # number
      num <- ''
      while (ch >= '0' && ch <= '9' && i <= length(sf)) {
        num <- paste0(num, ch)
        i <- i + 1
        ch <- sf[i]
      }
      out[[pos]] <- list(lr=lr, depth=depth, value=as.integer(num))
      pos <- pos + 1
    }
  }
  out
}

unparse_sf <- function(sf) {
  # find the maximum pair
  out <- sf
  while (length(out) > 1) {
    max_val <- max(map_int(out, 'depth'))
    # find these
    indexes <- which(map_int(out, 'depth') == max_val)
    if (length(indexes) >= 2) {
      # combine this pair ('explode' it down to a charvalue)
      left_pos <- indexes[1]
      right_pos <- indexes[2]
      pasted <- paste0('[', out[[left_pos]]$value, ',', out[[right_pos]]$value, ']')
      out <- c(out[seq_len(left_pos-1)], # left
               list(list(depth=max_val-1, lr='?', value = pasted)),
              out[right_pos+seq_len(length(out)-right_pos)])
    }
  }
  out[[1]]$value
}

check_parsing <- function(x) {
  x == parse_sf(x) |> unparse_sf()
}

check_parsing('[7,[6,[5,[4,[3,2]]]]]')
check_parsing('[[6,[5,[4,[3,2]]]],1]')
check_parsing('[[[[[9,8],1],2],3],4]')
check_parsing('[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]')
check_parsing('[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]')
check_parsing('[[[[0,7],4],[15,[0,13]]],[1,1]]')

# OK, now do an explode
explode <- function(sf) {
  # check if we have any depth=5
  needs_exploding <- which(map(sf, 'depth') == 5)
  if (length(needs_exploding) >= 2) {
    # explode!
    left_pos <- needs_exploding[1]
    right_pos <- needs_exploding[2]
    had_left <- FALSE
    if (left_pos > 1) {
      # add to the previous left pos
      sf[[left_pos-1]]$value <- sf[[left_pos-1]]$value + sf[[left_pos]]$value
      had_left <- TRUE
    }
    if (right_pos < length(sf)) {
      # add to the previous left pos
      sf[[right_pos+1]]$value <- sf[[right_pos+1]]$value + sf[[right_pos]]$value
    }
    # replace this pair with the number 0, i.e. drop down a depth
    sf <- c(sf[seq_len(left_pos-1)], # left
            list(list(depth=4, lr=ifelse(had_left, 'R', 'L'), value = 0)),
      sf[right_pos+seq_len(length(sf)-right_pos)]) # right
  }
  return(sf)
}

# ok do some exploding
parse_sf('[7,[6,[5,[4,[3,2]]]]]') |>
  explode() |> unparse_sf() # [7,[6,[5,[7,0]]]]
parse_sf('[[6,[5,[4,[3,2]]]],1]') |>
  explode() |> unparse_sf() # [[6,[5,[7,0]]],3]
parse_sf('[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]') |>
  explode() |> unparse_sf() # [[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]
parse_sf('[[3,[2,[8,0]]],[9,[5,[4,[3,2]]]]]') |>
  explode() |> unparse_sf() # [[3,[2,[8,0]]],[9,[5,[7,0]]]] # OK!

# splitting
split <- function(sf) {
  # any number > 10 splits
  needs_splitting <- which(map(sf, 'value') >= 10)
  if (length(needs_splitting) > 0) {
    # split the first item
    pos <- needs_splitting[1]
    val <- sf[[pos]]$value
    dep <- sf[[pos]]$depth
    left <- list(depth = dep+1, lr = 'L', value=floor(val/2))
    right <- list(depth = dep+1, lr = 'R', value=ceiling(val/2))
    sf <- c(sf[seq_len(pos-1)], # left
            list(left, right),
            sf[pos+seq_len(length(sf)-pos)])
  }
  return(sf)
}

sf <- parse_sf('[[[[0,7],4],[15,[0,13]]],[1,1]]')
split(sf) |> unparse_sf()

parse_sf('[[[[0,7],4],[15,[0,13]]],[1,1]]') |>
  split() |> unparse_sf()
parse_sf('[[[[0,7],4],[15,[0,13]]],[1,1]]') |>
  split() |> split() |> unparse_sf()

# check the example
(parse_sf('[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]') |>
  explode() |> explode() |>
  split() |> split() |> explode() |> unparse_sf()) == "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"

# yay, now do addition!
add <- function(sf_x, sf_y) {
  # increase the depth of sf_x by 1
  # increase the depth of sf_y by 1 and concat
  c(map(sf_x, \(x) { z = x; z$depth = z$depth+1; z }),
    map(sf_y, \(x) { z = x; z$depth = z$depth+1; z }))
}

# example with addition:
(add(parse_sf('[[[[4,3],4],4],[7,[[8,4],9]]]'), parse_sf('[1,1]')) |>
  explode() |> explode() |>
  split() |> split() |> explode() |> unparse_sf()) == "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"

# auto-reduce
reduce_sf <- function(sf) {
  # progressively explode then split
  out <- sf
  while (TRUE) {
    if (any(map(out, 'depth') == 5))
      out <- explode(out)
    else if (any(map(out, 'value') >= 10))
      out <- split(out)
    else
      break
  }
  return(out)
}

add_and_reduce <- function(x, y) {
  add(x, y) |>
    reduce_sf()
}

(add_and_reduce(parse_sf('[[[[4,3],4],4],[7,[[8,4],9]]]'), parse_sf('[1,1]')) |>
  unparse_sf()) == "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]"

# YAY!

# right, some complete examples
hw <- '[1,1]
[2,2]
[3,3]
[4,4]' |> str_split("\n") |> unlist()

# reduce this down
(map(hw, parse_sf) |> reduce(add_and_reduce) |>
  unparse_sf()) == "[[[[1,1],[2,2]],[3,3]],[4,4]]"

hw <- '[1,1]
[2,2]
[3,3]
[4,4]
[5,5]' |> str_split("\n") |> unlist()
(map(hw, parse_sf) |> reduce(add_and_reduce) |>
    unparse_sf()) == "[[[[3,0],[5,3]],[4,4]],[5,5]]"

hw <- '[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]
[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]
[[2,[[0,8],[3,4]]],[[[6,7],1],[7,[1,6]]]]
[[[[2,4],7],[6,[0,5]]],[[[6,8],[2,8]],[[2,1],[4,5]]]]
[7,[5,[[3,8],[1,4]]]]
[[2,[2,2]],[8,[8,1]]]
[2,9]
[1,[[[9,3],9],[[9,0],[0,7]]]]
[[[5,[7,4]],7],1]
[[[[4,2],2],6],[8,7]]' |> str_split('\n') |> unlist()
(map(hw, parse_sf) |> reduce(add_and_reduce) |>
    unparse_sf()) == "[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]"

# WOHOO!

# OK, finally let's compute the magnitude
magnitude <- function(sf) {
  out <- sf
  while (length(out) > 1) {
    max_val <- max(map_int(out, 'depth'))
    # find these
    indexes <- which(map_int(out, 'depth') == max_val)
    if (length(indexes) >= 2) {
      # combine this pair ('explode' it down to the magnitude
      left_pos <- indexes[1]
      right_pos <- indexes[2]
      mag <- out[[left_pos]]$value*3 + out[[right_pos]]$value*2
      out <- c(out[seq_len(left_pos-1)], # left
               list(list(depth=max_val-1, lr='?', value = mag)),
               out[right_pos+seq_len(length(out)-right_pos)])
    }
  }
  out[[1]]$value
}

magnitude(parse_sf('[[1,2],[[3,4],5]]'))
magnitude(parse_sf('[[[[8,7],[7,7]],[[8,6],[7,7]]],[[[0,7],[6,6]],[8,7]]]'))

hw <- '[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]' |> str_split('\n') |> unlist()

hw |> map(parse_sf) |> reduce(add_and_reduce) |> magnitude()

# OK, now part 1 (finally....)
readLines('2021/day18/input.txt') |>
  map(parse_sf) |>
  reduce(add_and_reduce) |>
  magnitude()

# part 2 is just a cross-join, right?
parsed <- tibble(hw=readLines('2021/day18/input.txt')) |>
  mutate(parsed = map(hw, parse_sf))

all_pairs <- parsed |> cross_join(parsed) |>
  filter(hw.x != hw.y)

# OK, now do the computation
all_pairs |>
  mutate(ans = map2(parsed.x, parsed.y, add_and_reduce)) |>
  mutate(mag = map_int(ans, magnitude)) |>
  slice_max(mag)
