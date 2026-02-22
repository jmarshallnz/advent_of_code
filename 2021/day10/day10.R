library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- "[({(<(())[]>[[{[]{<()<>>
[(()[<>])]({[<{<<[]>>(
{([(<{}[<>[]}>{[]{[(<()>
(((({<>}<{<{<>}{[]{[]{}
[[<[([]))<([[{}[[()]]]
[{[{({}]{}}([{[{{{}}([]
{<[[]]>}<{[{[{[]{()[[[]
[<(<(<(<{}))><([]([]()
<{([([[(<>()){}]>(<<{{
<{([{{}}[<[[[<>{}]]]>[]]" |> str_split("\n") |> unlist()

input <- readLines('2021/day10/input.txt')
lines <- input |> str_split('')


# we can use some sort of stack I think here?
library(collections)

parses_correct <- function(line) {
  openers <- c('(', '[', '{', '<')
  closers <- c(')', ']', '}', '>')

  s <- stack()
  # iterate through line, pushing to the stack on an open, popping on a close.
  for (i in 1:length(line)) {
    ch = line[i]
    if (ch %in% openers) {
      s$push(ch)
    } else {
      # closer. Check it's on the top of the stack
      wch_closer <- which(closers == ch)
      top = s$pop()
      wch_opener <- which(openers == top)
      if (wch_closer != wch_opener) {
        # b0rKed
        return(i)
      }
    }
  }
  return(0)
}

wrong <- map_int(lines, parses_correct)
wrong_closers <- map2_chr(lines, wrong, \(x, y) if (y) x[y] else NA)

# now lookup our score
scores <- c(')' = 3, ']' = 57, '}' = 1197, '>' = 25137)

scores[wrong_closers] |> sum(na.rm=TRUE)

# Part 2 we repair the lines
lines_to_repair <- lines[wrong == 0]

# ok, now repair using the same stack idea
repair_line <- function(line) {
  openers <- c('(', '[', '{', '<')
  closers <- c(')', ']', '}', '>')

  s <- stack()
  # iterate through line, pushing to the stack on an open, popping on a close.
  for (i in 1:length(line)) {
    ch = line[i]
    if (ch %in% openers) {
      s$push(ch)
    } else {
      # closer. Check it's on the top of the stack
      wch_closer <- which(closers == ch)
      top = s$pop()
    }
  }
  # reconstruct
  out <- character(s$size()); count = 1
  while(s$size()) {
    top = s$pop()
    wch_opener = which(openers == top)
    out[count] = closers[wch_opener]
#    cat("top is", top, "which is opener", wch_opener, "corresponding to", out[count], "\n")
    count = count+1
  }
  return(out)
}

repairs <- map(lines_to_repair, repair_line)


repair_scores <- map(repairs, \(x) repair_scores[x])

calc_score <- function(repair) {
  repair_scores <- c(')' = 1, ']' = 2, '}' = 3, '>' = 4)
  score = 0
  for (r in repair) {
    score <- score*5 + repair_scores[r]
  }
  return(score)
}
scores <- map_dbl(repairs, calc_score) |>
  sort()

scores[24]
