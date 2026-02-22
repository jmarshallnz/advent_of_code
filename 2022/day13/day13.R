library(tidyverse)

# we need to split by ',' ignoring [] first. Then we iterate
# process our list.
get_items <- function(s) {
  # scrub off the starting [ and ending ]
  s = s[-c(1, length(s))]
  # split by ',' accounting for nested sublists
  bracket_count = 0
  items = list()
  item_start = 1
  for (i in seq_along(s)) {
    if (s[i] == '[') {
      bracket_count = bracket_count + 1;
    }
    if (s[i] == ']') {
      bracket_count = bracket_count - 1;
    }
    if (bracket_count == 0 && s[i] == ',') {
      # have a comma so push back our item
      items[[length(items)+1]] <- s[item_start:(i-1)]
      item_start = i+1
    }
  }
  if (length(s) >= item_start) {
    items[[length(items)+1]] <- s[item_start:length(s)]
  }
  items
}

is_list <- function(s) {
  return(length(s) && s[1] == '[')
}

compare <- function(left, right) {
#  cat("comparing: ", paste(left, collapse=''), ' vs ', paste(right, collapse=''), '\n')
  if (is_list(left) || is_list(right)) {
    # lists left OR right, so get items and compare one by one
    litems <- if (is_list(left)) get_items(left) else list(left)
    ritems <- if (is_list(right)) get_items(right) else list(right)
 #   cat("have", length(litems), "and", length(ritems), "\n")
    # compare our litems with ritems one by one
    for (i in seq_len(min(length(litems), length(ritems)))) {
      d = compare(litems[[i]], ritems[[i]])
      if (d != 0) {
        return(d) # found a difference
      }
    }
    # no difference in the items, check lengths
    if (length(litems) < length(ritems))
      return(1)
    if (length(ritems) < length(litems))
      return(-1)
    # no difference in length
    return(0)
  }
  # not lists, so compare directly.
  litem = as.numeric(paste(left, collapse=''))
  ritem = as.numeric(paste(right, collapse=''))
  if (litem < ritem)
    return(1)
  if (litem > ritem)
    return(-1)
  return(0) # no difference?
}

input <- "[1,1,3,1,1]
[1,1,5,1,1]

[[1],[2,3,4]]
[[1],4]

[9]
[[8,7,6]]

[[4,4],4,4]
[[4,4],4,4,4]

[7,7,7,7]
[7,7,7]

[]
[3]

[[[]]]
[[]]

[1,[2,[3,[4,[5,6,7]]]],8,9]
[1,[2,[3,[4,[5,6,0]]]],8,9]" |> str_split('\n') |> unlist()

input <- readLines("2022/day13/input.txt")
check_order <- tibble(input = input) |>
  mutate(pair = cumsum(input == "")+1) |>
  filter(input != "") |>
  mutate(left = rep(c('l', 'r'), length.out = n())) |>
  pivot_wider(names_from=left, values_from=input) |>
  mutate(l = str_split(l, ''), r = str_split(r, '')) |>
  mutate(ans = map2_int(l, r, compare))

check_order |>
  filter(ans > 0) |>
  summarise(sum(pair))

# part 2: general sort. We've got a binary predicate, so just need a sort function?
# a quick ddg search for quicksort finds rje which ranks.

all <- c(input[input != ""], "[[2]]", "[[6]]") |>
  str_split('')

library(rje)
sorted <- quickSort(all, f = \(x, y) -compare(left=x, right=y)) # it's reverse...
sorted[301:302] |> prod()
