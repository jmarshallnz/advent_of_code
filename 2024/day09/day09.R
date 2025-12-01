library(tidyverse)

input <- "2333133121414131402"

input <- readLines("day9/input.txt")
# this is essentially run length encoding I think, and the first
# step is to undo it, which we can do with rep(x, times=foo)
d <- input |> str_split_fixed("", n=Inf) |>
  as.numeric()
files <- d[seq(1, length(d), by=2)]
blank <- d[seq(2, length(d), by=2)]

ids <- seq_along(files)-1
files_expand <- map2(ids, files, rep)
files_rev <- files_expand |> unlist() |> rev()
blank_fill <- map2(cumsum(blank), blank, ~files_rev[.x-.y+seq_len(.y)])
squished <- map2(files_expand[-length(files_expand)], blank_fill, c) |> unlist()
ans <- squished[1:length(files_rev)]

checksum <- sum(ans*(seq_along(ans)-1))
checksum

# part 2 we move files as blocks. So we don't need to rle them
# but do need to fill from the left which is a bit of a hassle
# the vectors aren't big here though so using match() is probably
# just fine (10k * 10k search)
cur_blank <- blank
filled_blank <- vector(mode="list", length=length(cur_blank))
for (i in rev(seq_along(files))) {
  wch_blank <- match(TRUE, cur_blank >= files[i])
  if (!is.na(wch_blank) && wch_blank < i) {
    filled_blank[[wch_blank]] <- c(filled_blank[[wch_blank]], rep(ids[i], files[i]))
    cur_blank[wch_blank] <- cur_blank[wch_blank] - files[i]
    files_expand[[i]] <- rep(0, length(files_expand[[i]])) # don't need this anymore
  }
}
# at the end, fill in the rest of the blanks with 0 to suit
filled_blank <- map2(filled_blank, blank, ~c(.x, rep(0, .y-length(.x))))

# ok ,now interleave and checksum
ans <- map2(files_expand, c(filled_blank, 0), c) |> unlist()
sum(ans*(seq_along(ans)-1))
