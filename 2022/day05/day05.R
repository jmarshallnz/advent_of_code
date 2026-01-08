library(tidyverse)

input <- read_fwf('2022/day05/input.txt', n_max=8)

stacks <- input |>
  mutate(across(everything(), \(x) substr(x, 2, 2))) |>
  as.list() |>
  map(\(x) rev(x[!is.na(x)]))

moves <- read.table('2022/day05/input.txt', skip=9) |>
  select(num=V2, from=V4, to=V6)

# ok, now run through our moves and process the stack

move <- function(stacks, from, to, num, rev=TRUE) {
  from_s <- stacks[[from]]
  moved <- from_s[length(from_s)+1 - num:1]
  if (rev) moved <- rev(moved)
  stacks[[from]] <- from_s[seq_len(length(from_s)-num)]
  stacks[[to]] <- c(stacks[[to]], moved)
  return(stacks)
}

moved_stacks <- stacks
for (i in 1:nrow(moves)) {
  moved_stacks <- move(moved_stacks, moves$from[i], moves$to[i], moves$num[i])
}

moved_stacks |>
  map_chr(last) |>
  as.list() |>
  do.call(paste0, args=_)

# part 2: don't reverse
moved_stacks <- stacks
for (i in 1:nrow(moves)) {
  moved_stacks <- move(moved_stacks, moves$from[i], moves$to[i], moves$num[i], rev=FALSE)
}

moved_stacks |>
  map_chr(last) |>
  as.list() |>
  do.call(paste0, args=_)
