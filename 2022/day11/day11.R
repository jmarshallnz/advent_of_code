library(tidyverse)

text <- "Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1" |> str_split("\n") |> unlist()

text <- readLines("2022/day11/input.txt")

start <- tibble(line = text) |>
  mutate(gap = line == "") |>
  mutate(monkey = cumsum(gap)) |>
  filter(line != "", !str_detect(line, "Monkey")) |>
  select(-gap) |>
  extract(line, into=c('name', 'value'), regex="(.*):(.*)") |>
  pivot_wider(names_from=name, values_from=value) |>
  rename(items = 2, op = 3, test = 4, true = 5, false = 6) |>
  extract(test, into='test', regex='([0-9]+)', convert=TRUE) |>
  extract(true, into='true', regex='([0-9]+)', convert=TRUE) |>
  extract(false, into='false', regex='([0-9]+)', convert=TRUE) |>
  mutate(items = str_split(items, ", ")) |>
  mutate(items = map(items, as.numeric)) |>
  nest(data=-monkey) |>
  pull(data)

start <- map(start, \(x) { x <- as.list(x); x$items <- x$items |> list_transpose(); x})

monkeys <- start
items_inspected <- numeric(length(monkeys))
for (round in 1:20) {
  # ok, now process a round
  for (i in 1:length(monkeys)) {
    monkey = monkeys[[i]]
    for (item in monkey$items) {
      # do the worry op
      old = item;
      eval(str2expression(monkey$op))
      # get bored
      priority = new%/%3
      # test if divisible
      pass_to_monkey <- if (priority %/% monkey$test * monkey$test == priority) monkey$true else monkey$false
      # pass to the next monkey
      monkeys[[pass_to_monkey+1]]$items <- c(monkeys[[pass_to_monkey+1]]$items, priority)
    }
    # done, so clear our list
    items_inspected[i] <- items_inspected[i] + length(monkey$items)
    monkeys[[i]]$items <- list()
  }
}

prod((items_inspected |> sort(decreasing=TRUE))[1:2])

# part 2: our worry amounts will blow out, but all we need is to know if it's
# divisible by each of the monkeys tests, so we can modulo down by that:
monkeys <- start
monkey_modulo <- monkeys |> map_int('test') |> prod()
items_inspected <- numeric(length(monkeys))
for (round in 1:10000) {
  # ok, now process a round
  for (i in 1:length(monkeys)) {
    monkey = monkeys[[i]]
    for (item in monkey$items) {
      # do the worry op
      old = item;
      eval(str2expression(monkey$op))
      # get bored: no division, but we can use module instead
      priority = new %% monkey_modulo
      # test if divisible
      pass_to_monkey <- if (priority %/% monkey$test * monkey$test == priority) monkey$true else monkey$false
      # pass to the next monkey
      monkeys[[pass_to_monkey+1]]$items <- c(monkeys[[pass_to_monkey+1]]$items, priority)
    }
    # done, so clear our list
    items_inspected[i] <- items_inspected[i] + length(monkey$items)
    monkeys[[i]]$items <- list()
  }
}

prod((items_inspected |> sort(decreasing=TRUE))[1:2])
