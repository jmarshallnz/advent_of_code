library(tidyverse)
library(fs)

term <- readLines("2022/day07/input.txt") |> str_split(' ')

# sanity check: how much total space do we have (i.e. at root?)
tibble(term=map_chr(term, \(x) paste(x, collapse=' '))) |>
  filter(!str_detect(term, '\\$'),
         !str_detect(term, 'dir')) |>
  extract(term, into='num', regex="([[:digit:]]+)", convert=TRUE) |>
  summarise(sum(num))

# hmm, how to process this efficiently? I guess we can just process line by
# line and summarise the ls() listings as we go recursively?

# the output is essentially <cd><ls> ... <cd><ls> ...
dir_sizes <- list()

cur_path <- character(0)

while (length(term)) {
  # pop the current term off
  command <- term[[1]]
  term <- term[-1]
  # process the command
  stopifnot(command[1] == "$") # error
  if (command[2] == 'cd') {
    # change our folder path
    cur_path = path_join(c(cur_path, command[3])) |>
      path_norm() |> path_split() |> unlist()
    cat("current path is:", paste(cur_path, collapse='/'), '\n')
    # insert into our dir_sizes list
    if (is.null(dir_sizes[[paste(cur_path, collapse='/')]]))
      dir_sizes[[paste(cur_path, collapse='/')]] <- 0
  }
  if (command[2] == 'ls') {
    # do a path listing
    while(length(term)) {
      item <- term[[1]]
      if (item[1] == "$")
        break;
      term <- term[-1]
      # process the item
      if (item[1] != "dir") {
        # file item, add to all our dirs upstream of this one
        size = as.numeric(item[1])
        # split up our path and add to all of them in turn
        dirs = accumulate(path_split(cur_path), \(x, y) paste(x, y, sep='/'))
        dir_sizes[dirs] <- map(dir_sizes[dirs], \(x) x + size)
      }
    }
  }
}

# part 1:
sizes <- tibble(size = dir_sizes |> unlist())
sizes |>
  filter(size <= 100000) |>
  summarise(sum(size))

# part 2 is now straight forward:
space_to_free <- dir_sizes[[1]]-40000000
sizes |>
  filter(size >= space_to_free) |>
  arrange(size)
