library(tidyverse)

# seems like a pretty straight-forward tree processing one
input <- "root: pppw + sjmn
dbpl: 5
cczh: sllz + lgvd
zczc: 2
ptdq: humn - dvpt
dvpt: 3
lfqf: 4
humn: 5
ljgn: 2
sjmn: drzm * dbpl
sllz: 4
pppw: cczh / lfqf
lgvd: ljgn * ptdq
drzm: hmdt - zczc
hmdt: 32" |> str_split("\n") |> unlist()

input <- readLines("2022/day21/input.txt")
all <- tibble(input = input) |>
  extract(input, into=c('monkey', 'func'), regex="(.*?): (.*)")

terminal <- all |> mutate(func = as.integer(func)) |> na.omit()

commands <- all |> anti_join(terminal |> select(monkey)) |>
  extract(func, into=c('left', 'op', 'right'), regex="([a-z]+) ([+\\-\\*/]) ([a-z]+)")

do_op <- function(op, left, right) {
  paste(left, op, right) |>
    map_dbl(\(x) eval(str2expression(x)))
}

# OK, now we build our evaluation tree and then do the computation?
# We can probably just iterate on 'solving' it as we can...
join_compute <- function(commands, terminal) {
  while(TRUE) {
    computable <- commands |>
      inner_join(terminal, by=join_by(left == monkey)) |>
      inner_join(terminal, by=join_by(right == monkey))
    cat("computable is:\n")
    print(computable)
    cat("\n")
    if (nrow(computable) <= 0)
      break # we're done or screwed...
    # do the computation
    new_term <- computable |> mutate(func = do_op(op, func.x, func.y)) |>
      select(monkey, func)
    # reduce our commands
    commands <- commands |> anti_join(computable)
    terminal <- terminal |> bind_rows(new_term)
    if (new_term |> filter(monkey == "root") |> nrow() > 0) {
      break # done!
    }
  }
  return(terminal)
}

join_compute(commands, terminal) |>
  filter(monkey == "root")

# ok, so part 2 is to work backwards. Given equality at root, what is the
# correct value at humn?

# we can solve this by basically noting that everything else is known EXCEPT humn
# and we need to solve for that.
terminal |> filter(monkey == "humn") # 1186 is current number

# first let's go and reduce the problem as much as we can
reduced <- terminal |> filter(monkey != "humn") # 1186 is current number

join_compute(commands, reduced)
nrow(reduced)
new_terminal <- join_compute(commands, reduced)
nrow(new_terminal) # ok, 1775
commands

done_commands <- commands |>
  inner_join(new_terminal, by=join_by(left == monkey)) |>
  inner_join(new_terminal, by=join_by(right == monkey))

new_commands <- commands |> anti_join(done_commands)
# ok, just 73 things to work with
need_to_solve <- new_commands |> left_join(new_terminal, by=join_by(left == monkey)) |>
  left_join(new_terminal, by=join_by(right == monkey))

# we know now what qpct is and can work backwards via solve:
qpct <- need_to_solve |> filter(monkey == "root") |> pull(func.y)

# we know what qpct is which is what that monkey will yell (the result of left <op> right)
# so we have to invert: qpct will be a terminal node and we'll swap the unknown to be the monkey
rearranged <- need_to_solve |>
  mutate(new_monkey = if_else(is.na(func.x), left, right)) |>
  mutate(new_op = case_when(op == '*' ~ '/',
                            op == '/' & is.na(func.x) ~ '*',
                            op == '+' ~ '-',
                            op == '-' & is.na(func.x) ~ '+',
                            TRUE ~ op)) |>
  mutate(new_left = if_else(new_op == op, left, monkey),
         new_right = case_when(is.na(func.x) ~ right,
                               new_op == op ~ monkey,
                               TRUE ~ left)) |>
  relocate(new_op, .after = 'new_left') |>
  select(monkey = new_monkey, left = new_left, op = new_op, right = new_right)

# forms are: monkey = left * foo =  -> monkey / foo = left (left is the unknown)
#            monkey = left / foo =  -> monkey * foo = left
#            monkey = left + foo =  -> monkey - foo = left
#            monkey = left - foo =  -> monkey + foo = left
# forms are: monkey = foo * right =  -> monkey / foo = right (right is the unknown)
#            monkey = foo / right =  -> foo / monkey = right ## <- gotcha!
#            monkey = foo + right =  -> monkey - foo = right
#            monkey = foo - right =  -> foo - monkey = right ## <- gotcha!

terminal_qpct <- tibble(monkey = "qpct", func=qpct) |>
  bind_rows(new_terminal)

# through it at part 1..
done <- join_compute(rearranged, terminal_qpct)
done |>
  filter(monkey == "humn")

# OK, let's check and see if that works!
fixed_terminal <-
  terminal |>
  mutate(func = if_else(monkey == "humn", 3592056845086, func))

join_compute(commands, fixed_terminal) # yup!
