library(tidyverse)

input <- read.table("2020/day09/input.txt", header=FALSE) |> pull(V1)

# OK, now check the numbers with a rolling 25 window: each one needs to be
# a sum of two previous
for (invalid in 26:length(input)) {
  prev <- input[invalid-(1:25)] # check our pair-wise sums
  found <- FALSE # see if it's found
  for (j in seq_len(length(prev)-1)) {
    for (k in (j+1):length(prev)) {
      if (input[invalid] == prev[j] + prev[k]) {
        found <- TRUE
        break
      }
    }
    if (found)
      break
  }
  if (!found)
    break
}
input[invalid]

# Part 2: find the contiguous length that adds to this.
# This is O(n^2) I guess?
ans <- 0
for (start in 1:(invalid-2)) {
  for (end in (start+1):(invalid-1)) {
    if (sum(input[start:end]) == input[invalid]) {
      # found it
      ans <- min(input[start:end]) + max(input[start:end])
    }
  }
}
