library(tidyverse)

options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines('2020/day14/input.txt') |>
  str_split_fixed(" = ", n=2) |> as.data.frame()

input <- 'mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0' |>
  str_split('\n') |> unlist() |>
  str_split_fixed(" = ", n=2) |> as.data.frame()

data <- input |> mutate(wch_mask = cumsum(V1 == "mask")) |>
  group_by(wch_mask) |> mutate(mask = first(V2)) |>
  filter(V1 != "mask") |>
  extract(V1, into="memory", regex="mem\\[([0-9]+)\\]", convert=TRUE) |>
  mutate(value = as.numeric(V2)) |>
  select(wch_mask, mask, memory, value) |>
  ungroup()

# OK, now convert value to binary and our masks accordingly
to_bits <- function(int) {
  to_bit <- function(x) {
    bit32 <- intToBits(x) |> as.integer() |> rev()
    c(rep(0L, 4), bit32)
  }
  map(int, to_bit)
}

to_int <- function(bits) {
  sum(2^(35:0)*bits)
}

ans <- data |> mutate(bin_val = to_bits(value)) |>
  mutate(mask_split = str_split(mask, '')) |>
  mutate(set1 = map(mask_split, \(x) x == '1')) |>
  mutate(set0 = map(mask_split, \(x) x == '0')) |>
  mutate(set_val = map2(bin_val, set1, \(x, y) if_else(y, 1L, x))) |>
  mutate(set_val = map2(set_val, set0, \(x, y) if_else(y, 0L, x))) |>
  mutate(set_val = map_dbl(set_val, to_int))

ans |> group_by(memory) |>
  summarise(val = last(set_val)) |>
  summarise(sum(val))

# Part 2: memory and values switch, plus X now means "both 0 and 1" meaning each memory
# writes to LOTS of memory simultaneously.
# We need to maintain a list of which memory locations are being written to in
# each step and _update_ all previous memory locations with their new numbers.

# e.g. X1010X
# then X1X10X will overwrite the previous state.
# Step 1 will be finding our memory locations (with the X's I guess?)
mem_sets <- data |> mutate(bin_mem = to_bits(memory)) |>
  mutate(mask_split = str_split(mask, '')) |>
  mutate(set1 = map(mask_split, \(x) x == '1')) |>
  mutate(setX = map(mask_split, \(x) x == 'X')) |>
  mutate(set_mem = map2(bin_mem, set1, \(x, y) if_else(y, 1L, x)))

# Step 2 is expanding out our memory locations. This can be done recursively based on setX I think?
floating_bits <- function(x, y) {
  # takes a 36 bitstream x and a set y and generates all possible y's and sets them
  # accordingly
  coms <- sum(y)
  fills <- map(1:2^coms-1, \(x) { y = intToBits(x) |> as.integer(); y[1:coms] })
  masks <- map(fills, \(z) { x[which(y)] = z; x })
  return(masks)
}

mem_sets |>
  mutate(set_mem_exp = map2(set_mem, setX, floating_bits)) |>
  select(wch_mask, value, set_mem, set_mem_exp) |>
  unnest(set_mem_exp) |>
  mutate(mem_add = map_dbl(set_mem_exp, to_int)) |>
  group_by(mem_add) |>
  summarise(v = last(value)) |>
  summarise(sum(v))
