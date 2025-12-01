library(tidyverse)

input <- read.table(text="190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20", sep=":", fill=TRUE)

answers <- input$V1
inputs <- input$V2 |> str_split(" ") |>
  map(as.numeric) |>
  map(na.omit)

# Ok, now read in our actual data
input <- read.table("day7/input.txt", sep=":", fill=TRUE)
answers <- input$V1
inputs <- input$V2 |> str_split(" ") |>
  map(as.numeric) |>
  map(na.omit)

# ok, so we need combinations of * and + that give the right answer

# can use either + or * in either slot, so there's 2^k possibilities for k things

operators <- map(inputs, ~rep(list(c('*', '+')), length(.)-1)) |> map(~expand_grid(!!!.)) |> map(t)

# OK, now we want to have these operators in turn run on our list

check_options <- function(operators, input) {
  # iterate over the operators
  reduce_eval <- function(accum, x, op) {
    ans <- str2expression(paste(accum, op, x)) |>
      eval()
  }
  map_dbl(seq_len(ncol(operators)), ~reduce2(input, operators[,.], reduce_eval))
}

foo <- tibble(answers = answers, operators = operators, inputs = inputs) |>
  mutate(possibles = map2(operators, inputs, check_options))

foo |>
  mutate(correct = map2_lgl(answers, possibles, ~any(.y == .x))) |>
  filter(correct) |>
  summarise(sum(answers))

# part 2: just add an operator. Problem is 3^10 >> 2^10 so we
#         probably have to be more efficient. I think do.call() will be faster
#         than str2expression() |> eval() by a significant margin...

operators <- map(inputs, ~rep(list(c('prod', 'sum', 'concat')), length(.)-1)) |> map(~expand_grid(!!!.)) |> map(t)

# OK, now we want to have these operators in turn run on our list

concat <- function(x, y) { as.numeric(paste0(x, y)) }

check_options <- function(operators, input, answer) {
  # iterate over the operators
  reduce_eval <- function(accum, x, op) {
    do.call(op, list(accum, x))
  }
  # mayaswell stop early...
  cat("working on answer=", answer, "\n")
  for (i in 1:ncol(operators)) {
    ans <- input[1]
    for (j in 2:length(input)) {
      ans <- reduce_eval(ans, input[j], operators[,i][j-1])
      if (ans > answer)
        break
    }
#    ans <- reduce2(input, operators[,i], reduce_eval)
    if (ans == answer) {
      return(TRUE)
    }
  }
  return(FALSE)
}

tibble(answers = answers, operators = operators, inputs = inputs) |>
  mutate(correct = pmap_lgl(list(operators, inputs, answers), check_options)) |>
  filter(correct) |>
  summarise(s = sum(answers)) |>
  pull(s)
