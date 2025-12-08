library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines('2023/day03/input.txt')

# find all the symbols, then all the numbers associated?
# or find all the numbers row, start, end and then check
# for symbol overlap? (could do that with a join?)

number_locs <- str_locate_all(input, pattern="([[:digit:]]+)") |>
  map(as.data.frame) |>
  bind_rows(.id = 'row') |>
  mutate(row = as.numeric(row)) |>
  mutate(number = as.numeric(str_sub(input[row], start, end))) |>
  mutate(y1 = row-1, y2 = row+1, x1 = start-1, x2 = end+1)

symbol_locs <- str_locate_all(input, pattern="([^0-9.]+)") |>
  map(as.data.frame) |>
  bind_rows(.id = 'row') |>
  mutate(row = as.numeric(row))

# OK, now do a join from number_locs to symbol_locs
number_locs |>
  tibble::rowid_to_column('number_id') |>
  left_join(symbol_locs, by = join_by(y1 <= row, y2 >= row,
                                      x1 <= start, x2 >= start)) |>
  filter(!is.na(row.y)) |>
  select(number_id, number) |>
  unique() |>
  summarise(sum(number))

# OK, extend this the other way around by finding only our "gears":
gear_locs <- str_locate_all(input, pattern="\\*") |>
  map(as.data.frame) |>
  bind_rows(.id = 'row') |>
  mutate(row = as.numeric(row))

gear_locs |>
  left_join(number_locs |> select(number, y1:x2), by = join_by(row >= y1, row <= y2,
                                      start >= x1, start <= x2)) |>
  add_count(row, start, end) |>
  filter(n == 2) |>
  group_by(row, start, end) |>
  summarise(ratio = first(number)*last(number)) |>
  ungroup() |>
  summarise(sum(ratio))

