library(tidyverse)
library(collections)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines('2020/day18/input.txt')

# We need to write a quick parser that builds an execution tree
line <- input[1] |> str_split('') |> unlist()

# The idea is that we need to go for "infix" to "postfix", noting
# that the operations are going to be _LEFT_ associative. i.e.
# ensure we always evaluate a * b * c as (a*b)*c.
# Thus, if precedence is equal, we always do the c operation
# after the a and b.

part1_precedence <- function(c) { return(1) } # same precedence
is_left_associative <- function(c) { TRUE }

infix_to_postfix <- function(line, prec_func, assoc_func = is_left_associative) {

  line <- str_split(line, '') |> unlist()

  post_fix <- list()   # our postfix expression
  op_stack <- stack() # our operator stack for precedence

  for (c in line) {
    # ignore white space
    if (c == ' ') {
      next
    }
    # if a number, add to our result
    if (c >= '0' && c <= '9') {
      post_fix[[length(post_fix)+1]] <- as.numeric(c)
    } else if (c == '(') {
      op_stack$push(c)
    } else if (c == ')') {
      # pop everything off our operator stack until we're back to )
      while (op_stack$size() && op_stack$peek() != '(') {
        post_fix[[length(post_fix)+1]] <- op_stack$pop()
      }
      # pop off the '(' as well
      op_stack$pop()
    } else {
      # must be + or *: we pop off the stack according to precedence.
      # this works as-is if everything is right-associative. But I think we want
      # left associativity here?
      # but, with + and * having the same precedence, it will be right-associative instead?
      while (op_stack$size() && op_stack$peek() != '(' &&
             ((prec_func(op_stack$peek()) > prec_func(c)) ||
             ((prec_func(op_stack$peek()) == prec_func(c) && is_left_associative(c))))) {
        post_fix[[length(post_fix)+1]] <- op_stack$pop()
      }
      # now push our current operator to the stack
      op_stack$push(c)
    }
  }

  # anything else on the operator stack can now be popped off
  while (op_stack$size()) {
    post_fix[[length(post_fix)+1]] <- op_stack$pop()
  }

  return(post_fix)
}

eval_postfix <- function(expression) {
  # idea is we push any number onto our stack. If we
  # find an operator, we pop the last two items off
  # the stack and do the operation, pushing the result
  # back onto the stack
  st <- stack()
  for (c in expression) {
    if (is.numeric(c)) {
      st$push(c)
    } else {
      # operator: pop our two values
      left = st$pop()
      right = st$pop()
      if (c == '+') {
        st$push(left + right)
      } else if (c == '*') {
        st$push(left * right)
      }
    }
  }
  return(st$peek())
}

tibble(input=input) |>
  mutate(parsed = map(input, \(x) infix_to_postfix(x, prec_func = part1_precedence))) |>
  mutate(ans = map_dbl(parsed, eval_postfix)) |>
  summarise(sum(ans))

# Part 2: change of operator precedence
part2_precedence <- function(c) { return(ifelse(c == '+', 2, 1)) } # + takes precedence

tibble(input=input) |>
  mutate(parsed = map(input, \(x) infix_to_postfix(x, prec_func = part2_precedence))) |>
  mutate(ans = map_dbl(parsed, eval_postfix)) |>
  summarise(sum(ans))


# ALTERNATE SOLUTION: Just use the parser, Luke.
# Idea is that a function has precedence higher than * (or +)
# Functions then have the same precedence

# For part 2:
'%+%' <- function(x, y) x+y
input |> str_replace_all('\\+', '%+%') |>
  map(str2expression) |>
  map_dbl(eval) |>
  sum()

# For part 1 we need both + and * to be new ops so they have the same precedence
'%+%' <- function(x, y) x+y
'%*%' <- function(x, y) x*y
input |> str_replace_all('\\+', '%+%') |>
  str_replace_all('\\*', '%*%') |>
  map(str2expression) |>
  map_dbl(eval) |>
  sum()

