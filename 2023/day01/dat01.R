library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

readLines('2023/day01/input.txt') |>
  str_split('') |>
  map(as.numeric) |>
  map(\(x) x[!is.na(x)]) |>
  map_dbl(\(x) first(x)*10+ last(x)) |>
  sum()

# not as simple as just replace digits with numbers: we need to replace the FIRST and LAST occurrence only!
# otherwise we might replace the 'two' in 'eightwo' first which would be bad.
# the trick then is to find the first of [digit] OR <letter thing>

word_matches <- tibble(num = 1:9) |> mutate(word = xfun::numbers_to_words(num)) |>
  select(word, num) |> mutate(num = as.character(num)) |>
  deframe()

# vectorised function for reversing a character string
str_rev <- function(x) {
  x |> map_chr(\(x) str_split(x, '') |> unlist() |> rev() |> paste(collapse=''))
}

tibble(line = readLines('2023/day01/input.txt')) |>
  mutate(pos = str_locate(line, paste(c(names(word_matches),"[0-9]"), collapse="|"))) |>
  mutate(start = str_sub(line, pos[,'start'], pos[,'end'])) |>
  mutate(first = as.numeric(str_replace_all(start, word_matches))) |>
  mutate(backwards = str_rev(line)) |>
  mutate(pos = str_locate(backwards, paste(c(str_rev(names(word_matches)),"[0-9]"), collapse="|"))) |>
  mutate(start = str_sub(backwards, pos[,'start'], pos[,'end'])) |>
  mutate(last = as.numeric(str_replace_all(start, word_matches |> set_names(str_rev(names(word_matches)))))) |>
  mutate(num = first*10+last) |>
  summarise(sum(num))
