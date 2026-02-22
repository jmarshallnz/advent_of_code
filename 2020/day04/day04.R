library(tidyverse)

input <- readLines('2020/day04/input.txt')

parsed <- tibble(input = input, gap = cumsum(input == "")) |>
  filter(input != "") |>
  mutate(sep_in = str_split(input, " ")) |>
  unnest(sep_in) |>
  extract(sep_in, into=c('field', 'value'), regex="([a-z]+):(.*)?")

parsed |>
  group_by(gap) |>
  summarise(n = n(), n_cid = sum(field == 'cid')) |>
  mutate(valid = n == 8 | (n == 7 & !n_cid)) |>
  filter(valid) |> nrow()

# part 2: need to check for valid fields now
parsed

#byr (Birth Year) - four digits; at least 1920 and at most 2002.
validate_byr <- function(value) {
  val <- as.numeric(value)
  val >= 1920 & val <= 2002
}

#iyr (Issue Year) - four digits; at least 2010 and at most 2020.
validate_iyr <- function(value) {
  val <- as.numeric(value)
  val >= 2010 & val <= 2020
}

#eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
validate_eyr <- function(value) {
  val <- as.numeric(value)
  val >= 2020 & val <= 2030
}

#hgt (Height) - a number followed by either cm or in:
#  If cm, the number must be at least 150 and at most 193.
#  If in, the number must be at least 59 and at most 76.
validate_hgt <- function(value) {
  num = str_extract(value, "([0-9]+)([a-z]+)", group=1) |> as.numeric()
  uni = str_extract(value, "([0-9]+)([a-z]+)", group=2)
  return((uni == "cm" && num >= 150 && num <= 193) ||
           (uni == "in" && num >= 59 && num <= 76))
}

#hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
validate_hcl <- function(value) {
  vals <- value |> str_split('') |> unlist()
  return(vals[1] == "#" && all(val[2:7] %in% c(0:9, letters[1:6])))
}

#ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
validate_ecl <- function(value) {
  return(value %in% c('amb', 'blu', 'brn', 'gry', 'grn', 'hzl', 'oth'))
}

#pid (Passport ID) - a nine-digit number, including leading zeroes.
validate_pid <- function(value) {
  val <- str_split(value, '') |> unlist() |> as.numeric()
  return(length(val) == 9 && all(val >= 0 & val <= 9))
}

validate_cid <- function(value) {
  return(TRUE)
}

#cid (Country ID) - ignored, missing or not.
parsed |>
  mutate(valid_fn = paste0('validate_', field, '("', value, '")')) |>
  mutate(valid = map_lgl(valid_fn, \(x) eval(str2expression(x)))) |>
  group_by(gap) |>
  summarise(n_valid = sum(valid), n_cid = sum(field == 'cid')) |>
  filter(n_valid == 8 | (n_valid == 7 & n_cid == 0)) |>
  nrow()
