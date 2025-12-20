library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

# day 19 is basically a binary decision tree with surrogate splits
# perhaps we can create one directly with the input?

# A quick look suggests this is going to be a pain in the arse,
# so probably better to just implement ourselves :(

input <- readLines("2023/day19/input.txt")
gap <- which(input == "")
workflows <- input[1:(gap-1)]
raw_value <- input[-c(1:gap)]

workflows <- "px{a<2006:qkq,m>2090:A,rfg}
pv{a>1716:R,A}
lnx{m>1548:A,A}
rfg{s<537:gd,x>2440:R,A}
qs{s>3448:A,lnx}
qkq{x<1416:A,crn}
crn{x>2662:A,R}
in{s<1351:px,qqz}
qqz{s>2770:qs,m<1801:hdj,R}
gd{a>3333:R,R}
hdj{m>838:A,pv}" |> str_split("\n") |>
  unlist()

nm <- sub("([^{]+).*", "\\1", workflows)
nm

process_condition <- function(cond) {
  cond |> str_split_fixed(":", n=2) |>
    as.data.frame() |>
    set_names('rule', 'then') |>
    mutate(then = if_else(then == '', rule, then),
           rule = if_else(rule == then, '', rule)) |>
    extract(rule, into=c('var', 'op', 'rhs'), regex="([xmas])(.*?)([0-9]+)", convert=TRUE)
}

flows <- sub(".*\\{(.*)\\}", "\\1", workflows) |>
  str_split(",") |> set_names(nm) |>
  map(process_condition)

# ok now run our values through our workflows
raw_value <- "{x=787,m=2655,a=1222,s=2876}
{x=1679,m=44,a=2067,s=496}
{x=2036,m=264,a=79,s=2244}
{x=2461,m=1339,a=466,s=291}
{x=2127,m=1623,a=2188,s=1013}"

values <- raw_value |>
  str_split('\n') |> unlist() |>
  str_replace("\\{", "c(") |>
  str_replace("\\}", ")") |>
  map(\(x) { eval(str2expression(x))})

apply_flow <- function(flow, value) {
  flow |>
    mutate(out = is.na(op) |
                (op == '<' & value[var] < rhs) |
                (op == '>' & value[var] > rhs)) |>
    filter(out) |>
    slice(1) |>
    pull(then)
}

out <- map_chr(values[1:200], \(value) {
  app_val1 <- flows |> map(\(x) apply_flow(x, value))
  reduce(seq_along(app_val1),
         \(x, y) { x = app_val1[[x]]; if (x %in% c('A', 'R')) done(x) else x }, .init='in')}
)
out <- .Last.value
values[which(out == "A")] |>
  map_dbl(sum) |> sum()

# Part 2:

# Now we need to work backwards from the accepted 'node' back
# through the tree, recording the _intersection_ of possible rows
flows

# hmm, start at in maybe?

#in -> px; if (s < 1351) qqz
#px -> A if (m > 2090); if s < 1351 AND m > 2090

# is the network tree-like if we ignore A and R?
flows |> enframe() |>
  unnest(value) |>
  select(name, then) |>
  filter(!then %in% c("A", "R")) |>
  as.matrix() |>
  graph_from_edgelist(directed=FALSE) |>
  plot(vertex.size=1, vertex.label = NA) # yes, it's tree-like

# OK, so it is tree like, so the number of paths between
# any pair of vertices is 1, so we don't have to worry about
# cycling

# the binary decision(s) at each node results in the space being
# partitioned. Each partition is disjoint.
# so all we need do is track the partitions at each node
# which consist of xmas[min] -> xmas[max]

space <- list(x = c(1, 4000),
              m = c(1, 4000),
              a = c(1, 4000),
              s = c(1, 4000))

# recursively compute the reduced space
reduced_space <- function(flow, space) {
  # termination
#  cat("at flow:", flow, "\n")
  if (any(map_dbl(space, diff) < 0)) {
    cat("terminating due to empty space\n")
    return(0)
  }
  if (flow == "A") {
#    cat("terminating at A\n")
    return(map_dbl(space, \(x) diff(x)+1) |>
      prod())
  }
  if (flow == "R") {
#    cat("terminating at R\n")
    return(0)
  }
  rows <- flows[[flow]] |> rowwise() |> group_split()
  total <- 0
  for (row in rows) {
    # reduce our space for this row
    var <- row$var
    left  <- space
    if (is.na(row$op)) {
      # no restriction, call directly
      total <- total + reduced_space(row$then, left)
    } else if (row$op == '<') {
      left[[var]][2] <- min(space[[var]][2], row$rhs-1)
      space[[var]][1] <- max(space[[var]][1], row$rhs)
      total <- total + reduced_space(row$then, left)
    } else if (row$op == '>') { # split space in two
      left[[var]][1] <- max(space[[var]][1], row$rhs+1)
      space[[var]][2] <- min(space[[var]][2], row$rhs)
      total <- total + reduced_space(row$then, left)
    }
  }
  return(total)
}

reduced_space("in", space)
