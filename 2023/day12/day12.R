library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines("2023/day12/input.txt") |>
  str_split_fixed(' ', n=2)

patterns <- input[,1] |> str_split('')
springs  <- input[,2] |> str_split(',') |>
  map(as.numeric)

patterns |>
  map_dbl(\(x) sum(x == "?")) |>
  mean() # mean of 10 2^10 isn't too bad I guess?

# so "dumb" method is just generate all possible patterns then filter
# them out
gen_all <- function(pattern) {
  if (length(pattern) == 0)
    return('')
  if (pattern[1] == "?") {
    c(paste0('#', gen_all(pattern[-1])),
      paste0('.', gen_all(pattern[-1])))
  } else {
    paste0(pattern[1], gen_all(pattern[-1]))
  }
}

all <- map(patterns, gen_all)

# sweet, now just see how many match the thing we want. We want
# to rle them basically?
rle_encode <- function(x) {
  x |> str_split('') |>
    map(rle) |>
    map(\(x) x$lengths[x$values == "#"])
}

all_rle <- all |>
  map(rle_encode)

# ok, now count our matches
all_rle[[1]]
springs[[1]]

count_matches <- function(rle_patterns, spring) {
  map_lgl(rle_patterns, \(x) length(x) == length(spring) && all(x == spring)) |>
  sum()
}

ans <- map2_int(all_rle, springs, count_matches)

# Part 2: Ofc the above is completely infeasible, as expected.
# suspect we need to partition the patterns.

# the '.' are separators for the #'s so how many of them
# there are doesn't matter. So we need the number of possibilities
# between each separator??

map(patterns, rle) |>
  map_dbl(\(x) max(x$lengths[x$value == "?"])) |>
  hist()

patterns

map_dbl(patterns, \(x) sum(x == ".")) |>
  hist() # ok, number of sequences to consider in each one. A handful have zero.

springmin <- map(springs, \(x) sum(x) + length(x)-1) # space needed for the springs

map2_dbl(springmin, patterns, \(x, y) length(y) - x) |>
  hist()

# ?      1
# ??     1 (2), 1
# ???    1 (3), 2 (2), 3, (1), [1,1] (1)
# ????   1 (4), 2 (3), 3, (2), 4 (1), [1,1] (3 = f(2,1)*[1]*f(1,1) + f(1,1)*[1]*f(2,1) + f(1,1)*[2]+f(1,1)), [1, 2] (1), [2, 1] (1)
# ?????  [1,1]: f(1, 1)*f(3,1) + f(2,1)*f(2,1)+ f(3,1)*f(1,1), [2, 1]: f(2,2)*f(2,1) + f(3,2)*f(1,1), [1,2]: f(1,1)*f(3,2)+f(2,1)*f(2,2)
#        [1,3]: f(1, 1)*f(3,3), [3, 1]: f(3,1)*f(1,1), [2,2]: f(2,2)*f(2,2); [1,1,1] f(1,1)*f(1,1)*f(1,1)
#

# If the first character is a # then it must consume the next spring exactly. This either fails or consumes the next bunch of ? and specifies
# a .
# If the first character is NOT a ? then the number is the number if it was a . (recurse in) or the number if it was a # (re-call function)?

count_springs <- function(pattern, spring) {
  if (length(spring) == 0) {
    if (any(pattern == "#")) {
      return(0) # nope, need more springs
    }
    return(1) # yes, we can fit "no" springs into anywhere
  }
  if (length(pattern) < sum(spring) + (length(spring)-1))
    return(0) # can't fit this in, so we have none
  if (pattern[1] == ".") {
    # stop, recurse in
    return(count_springs(pattern[-1], spring))
  }
  if (pattern[1] == "#") {
    # consume a spring (we're starting a spring)
    hit_spring <- pattern[1:spring[1]]
    if (any(hit_spring == '.'))
      return(0) # we can't consume this spring
    if (length(pattern) == spring[1]) # last spring?
      return(ifelse(length(spring == 1), 1, 0)) # matched the last spring
    if (pattern[spring[1]+1] == "#") # we can't consume this spring
      return(0)
    return(count_springs(pattern[-(1:(spring[1]+1))], spring[-1]))
  }
  # either this is a '.' where by we iterate in OR it's a #
  ans <- count_springs(c('.', pattern[-1]), spring) +
    count_springs(c('#', pattern[-1]), spring)
  return(ans)
}

library(memoise)
count_springs <- memoise(count_springs)

# ok, this should now work.
long_patterns <- map(patterns, \(x) { rep(c('?', x), 5)[-1]})
long_springs  <- map(springs, \(x) rep(x, 5))

map2_dbl(long_patterns, long_springs, count_springs) |>
  sum()
