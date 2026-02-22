library(tidyverse)
library(collections)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines('2020/day19/input.txt')

input <- '0: 4 1 5
1: 2 3 | 3 2
2: 4 4 | 5 5
3: 4 5 | 5 4
4: "a"
5: "b"

ababbb
bababa
abbbab
aaabbb
aaaabbb' |> str_split('\n') |> unlist()

input <- '42: 9 14 | 10 1
9: 14 27 | 1 26
10: 23 14 | 28 1
1: "a"
11: 42 31
5: 1 14 | 15 1
19: 14 1 | 14 14
12: 24 14 | 19 1
16: 15 1 | 14 14
31: 14 17 | 1 13
6: 14 14 | 1 14
2: 1 24 | 14 4
0: 8 11
13: 14 3 | 1 12
15: 1 | 14
17: 14 2 | 1 7
23: 25 1 | 22 14
28: 16 1
4: 1 1
20: 14 14 | 1 15
3: 5 14 | 16 1
27: 1 6 | 14 18
14: "b"
21: 14 1 | 1 14
25: 1 1 | 1 14
22: 14 14
8: 42
26: 14 22 | 1 20
18: 15 15
7: 14 5 | 1 21
24: 14 1

abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa
bbabbbbaabaabba
babbbbaabbbbbabbbbbbaabaaabaaa
aaabbbbbbaaaabaababaabababbabaaabbababababaaa
bbbbbbbaaaabbbbaaabbabaaa
bbbababbbbaaaaaaaabbababaaababaabab
ababaaaaaabaaab
ababaaaaabbbaba
baabbaaaabbaaaababbaababb
abbbbabbbbaaaababbbbbbaaaababb
aaaaabbaabaaaaababaa
aaaabbaaaabbaaa
aaaabbaabbaaaaaaabbbabbbaaabbaabaaa
babaaabbbaaabaababbaabababaaab
aabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba' |>
  str_split('\n') |> unlist()

gap <- which(input == "")
rule_in <- input[seq_len(gap-1)]
messages <- input[(gap+1):length(input)]

rule_in

# construct our rule trees? We have both concatenation and ORs as operations, so
# each node in the tree is either:
# 1. A letter (a or b)
# 2. One or more concatenated rules.
# 3. Concatenated rules combined with an OR operation.
# This could be a list(val=, or=list(c(subrules), c(subrules), ...))
parse_rule <- function(rule) {
  # check for letter
  if (grepl('"[ab]"', rule)) {
    return(list(val=str_sub(rule, 2, 2)))
  }
  # check if we have a |
  subrules <- str_split(rule, " \\| ") |> unlist()
  or=list()
  for (sub in subrules) {
    # split the subrule(s) by " " and convert to rule numbers
    rule_nums <- str_split(sub, ' ') |> unlist() |> as.numeric()
    or[[length(or)+1]] = rule_nums+1
  }
  return(list(or=or))
}

ruleset <- tibble(rule=rule_in) |>
  extract(rule, into=c('num', 'rule'), regex="([0-9]+): (.*)", convert=TRUE) |>
  mutate(num = num+1) |>
  mutate(rule = map(rule, parse_rule))

rules <- list()
for (i in 1:nrow(ruleset)) {
  rules[[ruleset$num[i]]] <- ruleset$rule[[i]]
}

# OK, now apply our rules
apply_rule <- function(rules, num, message) {

  if (length(message) == 0)
    return(0) # no message can possibly work

  rule <- rules[[num]]
 # cat('checking rule', num, 'against', message, '\n')
  if (!is.null(rule$val)) {
    # check if the message matches this character
 #   if (message[1] == rule$val)
#      cat("rule", num, "satisfied with length", 1, "\n")
    return(ifelse(message[1] == rule$val, 1, 0))
  }

  # have subrules, so evaluate them one by one
  # we have to check them all regardless of whether one is satified
  # for part 2
  all_results <- c()
  for (sub in rule$or) {
    # check if this list of subrules is satisfied
#    submessages <- list(message) # start with the normal message
    results <- 0
    for (subsub in sub) {
      next_results <- c()
      for (res in results) {
        submessage <- if(res > 0) message[-seq_len(res)] else message
        #    cat("message is:", message, "\n")
        #     cat("submessage for result", result, "is", submessage, "\n")
        subres <- apply_rule(rules, subsub, submessage)
        # could have multiple subres: add them all to our search list
        if (!all(subres == 0)) {
          # rule is satisfied, so a possibility for the next subsub
          next_results <- c(next_results, subres + res)
        }
      }
      # check if we have any next_results at all
      if (length(next_results) == 0) {
        # nope
        results = 0
        break
      }
      results <- next_results
    }
    if (any(results != 0)) {
      # satified a set of rules
#      cat("rule satisfied for rule", num, 'with length', results |> unlist(), '\n')
      all_results <- c(all_results, results)
    }
    # NOW, we ALSO need to check the alternate branch.
  }
  if (any(all_results != 0)) {
    return(all_results)
  }
  return(0) # not satisfied
}

valid_message <- function(message, ruleset=rules) {
  message = message |> str_split('') |> unlist()
  return(any(apply_rule(ruleset, 1, message) == length(message)))
}

messages |> map_lgl(valid_message) |> sum()

# Part 2: We need to add loops to rule 8 and 11, which for us is rules 9 and 12:
rules[[9]]
rules[[12]]

# This changes the rules from 9 -> 43 to 9 -> 43 | 43 9. This is basically "match 43 exactly, or multiples of 43"
# and rule 12 from 12: 43 32 to 12: 43 32 | 43 12 32. This is basically "match 43 then 32, or multiples of 43 then multiples of 32?
# i.e. it will match 43 32 OR 43 (43 32) 32 OR 43 43 (43 32) 32 32 etc.
# i.e. same number of 43s followed by a 32.

# What do 43 and 32 hit?

# 43 hits rules 14 (b) and 93 (ba) or rules a and 11 (((a (aabb OR abXa) OR b (aba)) OR b 53) b OR 41 a) a OR 47 b).

# 43 hits bba OR very long thing[]

# 32 hits rules 14 (b) and 37 (b 50 or a 25) or rules a and 16.

# Our above code will need to be altered to allow branching of lengths
newrules <- rules
newrules[[9]] <- list(or = list(43, c(43, 9)))
newrules[[12]] <- list(or = list(c(43, 32), c(43, 12, 32)))

for (i in 1:length(messages)) {
  cat('trying message', i, '\n')
  valid_message(messages[i], rules=newrules)
}
messages[2]

ans <- map_lgl(messages, \(x) valid_message(x, rules=newrules)) # hmm, too low?
