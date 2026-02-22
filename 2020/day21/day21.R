library(tidyverse)
library(collections)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines('2020/day21/input.txt')

input <- 'mxmxvkd kfcds sqjhc nhms (contains dairy, fish)
trh fvjkl sbzzf mxmxvkd (contains dairy)
sqjhc fvjkl (contains soy)
sqjhc mxmxvkd sbzzf (contains fish)' |>
  str_split('\n') |> unlist()

all <- tibble(input=input) |>
  extract(input, into=c('ingredients', 'allergens'), regex="(.*) \\(contains (.*)\\)") |>
  tibble::rowid_to_column('food') |>
  mutate(ingredients = str_split(ingredients, " "),
         allergens = str_split(allergens, ", ")) |>
  unnest(allergens)

# Part 1: If an ingredient has an allergen it has to appear in all lines that have
# that allergen listed.
not_possible <- all |> unnest(ingredients) |>
  group_by(allergens) |>
  mutate(num = n_distinct(food)) |>
  group_by(allergens, num) |>
  count(ingredients) |>
  group_by(ingredients) |>
  summarise(OK = all(n < num)) |>
  filter(OK)

# OK, now count these
all |> unnest(ingredients) |> semi_join(not_possible) |>
  group_by(ingredients) |>
  summarise(count = n_distinct(food)) |>
  summarise(sum(count))

# Part 2: remove these allergens

allergens <- all |> unnest(ingredients) |> anti_join(not_possible) |>
  group_by(food, allergens) |>
  nest(ingredients = ingredients) |>
  mutate(ingredients = map(ingredients, \(x) x |> pull(ingredients))) |>
  group_by(allergens) |>
  summarise(ingredients = list(reduce(ingredients, intersect))) |>
  deframe()

while(any(lengths(allergens) > 1)) {
  # find the one with length 1
  wch <- which(lengths(allergens) == 1)
  matched_ingredients <- allergens[wch] |> unlist()
  # rule this out from everything else
  allergens[-wch] <- map(allergens[-wch], \(x) setdiff(x, matched_ingredients))
}

allergens |> unlist() |> paste(collapse=',')
