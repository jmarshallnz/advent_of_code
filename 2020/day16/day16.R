library(tidyverse)

options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines('2020/day16/input.txt')

rules <-
my_ticket <- input[23]
others <- input[26:length(input)]

# parse our rules
rules <- input[1:20] |> str_extract_all('([0-9]+)') |> map_dfr(\(x) { x <- as.numeric(x); data.frame(s1=x[1], e1=x[2], s2=x[3], e2=x[4]) })



others <- str_split_fixed(input[26:length(input)], ',', n=Inf) |>
  apply(2, as.numeric)

# OK for each row in others, check it satisfies all the rules
invalids <- list()
for (i in 1:nrow(others)) {
  other <- others[i,]
  valid_against_rule <- matrix(FALSE, nrow=nrow(rules), ncol=ncol(others))
  for (j in 1:nrow(rules)) {
    rule <- rules |> slice(j)
    valid_against_rule[j,] <- (other >= rule$s1 & other <= rule$e1) |
             (other >= rule$s2 & other <= rule$e2)
  }
  valid <- apply(valid_against_rule, 2, any)
  wch_valid <- which(!valid)
  for (w in wch_valid) {
    invalids[[length(invalids)+1]] <- data.frame(i=i, invalid=other[w])
  }
}
invalids |> bind_rows() |> summarise(sum(invalid))

# part 2: throw away the ones we know are bad
bad <- invalids |> bind_rows() |> pull(i)

nearby <- others[-bad,]

# Now figure out which field is which: the idea is that nearby[,k] must
# match one of the fields. It might match more than one, but that's what
# we need to figure out
possible_rules <- matrix(FALSE, nrow=ncol(nearby), ncol=nrow(rules))
for (i in 1:ncol(nearby)) {
  valid_vals <- nearby[,i]
  # valid_vals must ALL match one rule
  for (j in 1:nrow(rules)) {
    rule <- rules |> slice(j)
    # check if all these values are valid for this rule
    if (all((valid_vals >= rule$s1 & valid_vals <= rule$e1) |
            (valid_vals >= rule$s2 & valid_vals <= rule$e2))) {
      # yes, all values validate successfully against this rule
      possible_rules[i, j] <- TRUE
    }
  }
}

# OK, now let's figure out which ones are really possible
possibles <- possible_rules |> apply(1, sum)
sort(possibles) == 1:length(possibles)

rownames(possible_rules) <- 1:nrow(possible_rules)
colnames(possible_rules) <- 1:ncol(possible_rules)

# Nice!
rule_map <- list()
while(TRUE) {
  if (nrow(possible_rules) == 0)
    break
  possibles <- possible_rules |> apply(1, sum)
  # find the one we can nail down
  wch_row <- which(possibles == 1)
  wch_col <- which(possible_rules[wch_row,])
  rule_map[[length(rule_map)+1]] <- data.frame(from_nearby=names(possibles)[wch_row], to_rule=colnames(possible_rules[wch_row,,drop=FALSE])[wch_col])
  possible_rules <- possible_rules[-wch_row, -wch_col, drop=FALSE]
}

rule_map |> bind_rows()

# find our departure rules
departure_rules <- which(input[1:20] |> str_detect("departure"))

departure_pos <- rule_map |> bind_rows() |>
  filter(to_rule %in% departure_rules) |>
  pull(from_nearby) |> as.numeric()

my_ticket <- input[23] |> str_split(',') |> unlist() |> as.numeric()
my_ticket[departure_pos] |> prod()
