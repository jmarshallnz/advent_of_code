library(tidyverse)

lines <- read.table("2020/day08/input.txt", sep=' ') |>
  rename(op = V1, off = V2) |>
  rowwise() |> group_split()

# OK, accumulator starts at 0, line number starts at 1
# we loop until we hit the same line number

run_program <- function(lines) {
  acc <- 0
  lin <- 1
  prev_lines <- rep(FALSE, length(lines))
  while(lin <= length(lines)) {
    # check if this line has been hit before
    if (prev_lines[lin]) {
      break
    }
    # run the line
    line <- lines[[lin]]
    next_lin <- lin+1
    if (line$op == "acc") {
      acc <- acc+line$off
    } else if (line$op == "jmp") {
      next_lin <- lin + line$off # jmp
    }
    prev_lines[lin] <- TRUE
    lin <- next_lin
  }
  return(lst(lin, acc))
}

run_program(lines)$acc

# Part 2: We need to find which line to correct. Worst case it's O(length(lines)^2) which is fine
for (i in 1:length(lines)) {
  # "fix" this line
  fixed_lines <- lines
  if (lines[[i]]$op == "jmp") {
    fixed_lines[[i]]$op <- "nop"
  } else if (lines[[i]]$op == "nop") {
    fixed_lines[[i]]$op <- "jmp"
  }
  ans <- run_program(fixed_lines)
  if (ans$lin > length(lines)) {
    break # done!
  }
}
ans
