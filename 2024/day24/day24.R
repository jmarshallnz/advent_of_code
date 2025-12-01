library(tidyverse)
library(bigBits)
library(ggraph)

input <- read_lines("day24/input.txt")

in_wires <- input[1:(which(input == "")-1)] |>
  str_split_fixed(": ", n=Inf) |> as.data.frame() |> set_names('wire', 'value') |>
  mutate(value = as.integer(value))

gates <- input[(which(input=="")+1):length(input)] |>
  str_split_fixed(" ", n=Inf) |> as.data.frame()

out_wires <- gates$V5

ops <- gates |> select(a=V1,op=V2,b=V3) |> rowwise() |> group_split() |>
  set_names(nm=out_wires)

# add our knowns
figure <- names(ops)[str_starts(names(ops), "z")] |>
  sort()

# ok, now just recursively evaluate
eval <- function(out) {
  if (any(in_wires$wire == out)) {
    return(in_wires |> filter(wire == out) |> pull(value))
  }
  op <- ops[[out]]
  switch(op$op,
         OR = eval(op$a) || eval(op$b),
         XOR = eval(op$a) != eval(op$b),
         AND = eval(op$a) && eval(op$b))
}

# part 1
map_lgl(rev(figure), eval) |>
  as.integer() |> paste(collapse='') |> base2base(frombase=2, tobase=10)

# Part 2 feels HARD. We have an addition machine, but it produces the wrong answer.

# it needs to work for all input, so presumably needs to be a well-designed adder?
# there's probably not many options for that?!? e.g. it's an XOR for x00,y00->z00
# then you've got carry bits etc (AND?)

# there are 222 gates, of which 4 pairs might need swapping. So we'd need to pick 8 of the 222
# to swap. This is much too big a number to compute with, so utilising
# our knowledge of a binary adder seems the way to go

# let's draw a network to see if it could be a bunch of adders:
network <- bind_rows(gates |> select(V1, V5),
                     gates |> select(V1=V3, V5)) |>
  as.matrix() |>
  graph_from_edgelist()

ggraph(network, layout=layout_as_tree(network)) +
  geom_edge_link(arrow = arrow(angle=10,length=unit(3, 'mm'))) +
  geom_node_label(aes(label=name), label.size=0.1, size=1)

# it looks like all the inputs are on top and all the z's
# are in row 2 as a guess? Hard to see.

# let's check our network depth? how many things are up and down from our output to get a sense of complexity
depth <- function(out, func=max) {
  if (any(in_wires$wire == out)) {
    return(0)
  }
  op <- ops[[out]]
  1+func(depth(op$a),depth(op$b))
}

map_int(figure, depth, func=max) # hmm some are a long way up, but there's a pattern?
# e.g. it's pretty clear here that we have a couple errors about half way along  as we have evens
# and then a 1 and then a bunch of odds...
map_int(figure, depth, func=min) # minimum depth is 2 back to an x/y. I think that
# works for a standard adder, which would be:

# out = (x XOR y) XOR carry_in
# carry_out = (x AND y) OR (carry_in AND (x XOR y))

# the otuput is always just two XORs from input. But could be a long way from input with the carry chain
# it makes sense that it's probably even though?

# so to solve I think we can just check if this is the arrangement and then fix it?

# order the operands to find matching faster
initial_gates <- input[(which(input=="")+1):length(input)] |>
  str_split_fixed(" ", n=Inf) |> as.data.frame() |> mutate(a = pmin(V1, V3),
                         b = pmax(V1, V3),
                         op = V2,
                         out = V5) |>
  select(a,b,op,out)

# routine to swap our gates
swap_gates <- function(gates, s1, s2) {
  # swap s1 and s2
  gates |>
    mutate(out = case_when(out == s1 ~ s2,
                           out == s2 ~ s1,
                           TRUE ~ out))
}

gates <- initial_gates |>
  swap_gates("jss", "rds") |>
  swap_gates("z08", "mvb") |>
  swap_gates("z18", "wss") |>
  swap_gates("z23", "bmn")

half_adders <- function(gates) {
  ands <- gates |> filter(op == 'AND') |> select(a, b, AND = out)
  xors <- gates |> filter(op == 'XOR') |> select(a, b, XOR = out)
  # these should match:
  if (ands |> anti_join(xors) |> nrow() +
      xors |> anti_join(ands) |> nrow() > 0)
    stop("and and xor gates don't match")
  ands |> left_join(xors)
}

ha <- half_adders(gates)
in_ha <- ha |> filter(str_starts(a, "x"))

# the output from our in_ha should be the b input for out_ha. the a input will be the carry bit
out_ha <- ha |> anti_join(in_ha) |>
  mutate(bin = if_else(b %in% in_ha$XOR, b, a),
         ain = if_else(b %in% in_ha$XOR, a, b)) |>
  select(a=ain, b=bin, AND, XOR)

# check the output lines up
in_ha |> anti_join(out_ha, by=join_by(XOR == b))
out_ha |> anti_join(in_ha, by=join_by(b == XOR))

# ok, our half adders are correctly hooked together, let's check the outputs
both_adders <- in_ha |> left_join(out_ha |> rename(out = XOR, carry_in=a, carry_AND = AND), by=join_by(XOR == b)) |>
  arrange(a)

# ok, now check our output should match the input
both_adders |> filter(str_sub(a, 2,3) != str_sub(out, 2,3))
# ok, looks like z08 is in the carry AND instead of in the out, try swapping z08 and mvb?
gates |> filter(a == 'mvb' | b == 'mvb') # mvb is feeding an OR, so that is correct
# ok, looks like z18 is in the first AND output and it hsould be wss?
gates |> filter(a == "wss" | b == "wss") # wss is feeding an OR
# ok, now it looks like z23 is incorrect maybe?
gates |> filter(a == "bmn" | b == "bmn") # bmn is the output of a halfadder
# and feeding an input into a halfadder with output z24

# let's grab our OR and see where they are
or_gates <- gates |> filter(op == "OR") |>
  mutate(bin = if_else(b %in% in_ha$AND, b, a),
         ain = if_else(b %in% in_ha$AND, a, b)) |>
  select(a=ain, b=bin, carry_out = out)

# see if our or_gates are correct. b should hook up to the AND gate
# and a to the CARRY_AND
or_gates |> anti_join(both_adders, by=join_by(a == carry_AND, b == AND)) # yes?
both_adders |> anti_join(or_gates, by=join_by(carry_AND == a, AND == b)) # no, but OK

full_adders <- both_adders |> left_join(or_gates, by=join_by(carry_AND == a, AND == b))

# our carry out should map to carry in. It doesn't for z24, but swappign z23 and bmn should
# do the trick?
full_adders |> filter(lag(carry_out) != carry_in)

# ok, so we have our set
all_swap_gates <- c("jss", "rds", "z08", "mvb", "z18", "wss", "z23", "bmn") |>
  sort() |>
  paste(collapse=",")
all_swap_gates
