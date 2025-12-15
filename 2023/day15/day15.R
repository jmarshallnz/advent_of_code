library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7" |>
  str_split(',') |>
  unlist()

hash <- function(s) {
  h <- function(h, ch) {
    h <- h + ch
    h <- h * 17
    h <- h %% 256
    h
  }
  as <- as.integer(charToRaw(s))
  reduce(as, h, .init=0)
}

input <- readLines("2023/day15/input.txt") |>
  str_split(',') |> unlist()

map_int(input, hash) |> sum()

# Part 2: seems straightforward processing exercise?
# boxes are a list containing label, number pairs in order.
# an operation LABEL=NUM replaces the existing LABEL=NUM pair
# or adds it if it's not there.
# an operation LABEL-NUM removes the lens from the list.
# lists in R already contain an order, so this should be
# straightforward, right? RIGHT?

label <- input |> str_split("[=-]")
types <- ifelse(grepl("\\-", input), "-", "=")

steps <- map2(label, types, \(x, y) data.frame(label=x[1], event=y, number=as.numeric(x[2])))

process_step <- function(boxes, step) {
  box <- hash(step$label) + 1
  if (step$event == "=") { # add to box
    boxes[[box]][[step$label]] <- step$number
  } else { # remove from box
    if (!is.null(boxes[[box]][step$label])) {
      boxes[[box]][[step$label]] <- NULL # seems to be making boxes smaller?
    }
  }
  boxes
}

focus <- function(box) {
  b <- box |> unlist()
  sum(b*(1:length(b)))
}

boxes <- reduce(steps, process_step, .init=vector("list", 256))
box_foc <- map(boxes, focus) |>
  unlist()
sum(box_foc * 1:length(box_foc))
