library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- readLines('2021/day08/input.txt') |>
  str_split_fixed('[ |]+', n=14)

input <- "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb | fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec | fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef | cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega | efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga | gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf | gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf | cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd | ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg | gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc | fgae cfgab fg bagce" |>
  str_split("\n") |> unlist() |>
  str_split_fixed('[ |]+', n=14)

out <- input[,11:14]

nc_out <- out |> nchar()
sum(nc_out %in% c(2, 3, 4, 7))

# part 2 is the full deduction
input[1,]

nc_in <- input[,1:10] |> nchar()

# we know the 1 (2), 7 (3), 4 (4) 8 (7). From that we can easily
# work out the others I think?
one <- apply(input[,1:10], 1, \(x) x[nchar(x) == 2])
seven <- apply(input[,1:10], 1, \(x) x[nchar(x) == 3])
four <- apply(input[,1:10], 1, \(x) x[nchar(x) == 4])
eight <- apply(input[,1:10], 1, \(x) x[nchar(x) == 7])

# ok, the unknown ones are the 5's (2, 3, 5) and 6's (6, 9, 0)
# 4 is a strict subset of 9 so we can identify the 9 using that.
# 3 vs 2/5 is done with overlap with 1 (1 is a strict subset of 3).
# that leaves 2 vs 5 and 6 vs 0.
# 5 is a subset of 6 but not 0 so we should be able to distinguish the rest using that?
snz <- apply(input[,1:10], 1, \(x) x[nchar(x) == 6], simplify = FALSE)
ttf <- apply(input[,1:10], 1, \(x) x[nchar(x) == 5], simplify = FALSE)

is_subset <- function(s1, s2) {
  s1 <- str_split(s1, '') |> unlist()
  s2 <- str_split(s2, '') |> unlist()
  all(s1 %in% s2) || all(s2 %in% s1)
}

# 4 is a subset of 9:
wch_9 <- map2(snz, four, \(x, y) map_lgl(x, \(z) is_subset(z, y)))
nine <- map2_chr(snz, wch_9, \(x, y) x[y])
sz <- map2(snz, wch_9, \(x, y) x[!y])

# 1 is a subset of 3:
wch_3 <- map2(ttf, one, \(x, y) map_lgl(x, \(z) is_subset(z, y)))
three <- map2_chr(ttf, wch_3, \(x, y) x[y])
tf <- map2(ttf, wch_3, \(x, y) x[!y])

# need to cross-join really: the entry in tf that overlaps with an entry
# in sz is 5 (which maps to 6)
wch_1 <- map2(tf, sz, \(x, y) map_lgl(x, \(z) is_subset(z, y[1])))
wch_2 <- map2(tf, sz, \(x, y) map_lgl(x, \(z) is_subset(z, y[2])))
wch_6 <- map2(wch_1, wch_2, \(x, y) { if (any(x)) 1 else 2 }) # only one of these will hit. If wch_1 hits, then sz[1] is 6, sz[2] is 0. If wch_2 hits
# pull out the 6
six <- map2_chr(sz, wch_6, \(x, y) x[y])
zero <- map2_chr(sz, wch_6, \(x, y) x[-y])
# pull out the five
wch_5 <- map2(wch_1, wch_2, \(x, y) { if (any(x)) which(x) else which(y) }) # only one of these will hit. If wch_1 hits, then sz[1] is 6, sz[2] is 0. If wch_2 hits
five <- map2_chr(tf, wch_5, \(x, y) x[y])
two <- map2_chr(tf, wch_5, \(x, y) x[-y])

# OK, now we can decode
# we need only identify which of the outputs correspond to which input, once we sort them.
digits <- list(zero, one, two, three, four, five, six, seven, eight, nine) |>
  list_transpose()

identical <- function(s1, s2) {
  s1 <- str_split(s1, '') |> unlist()
  s2 <- str_split(s2, '') |> unlist()
  length(s1) == length(s2) && all(sort(s1) == sort(s2))
}

thou <- map2_int(input[,11], digits, \(x, y) which(map_lgl(y, \(z) identical(x, z))))-1
hund <- map2_int(input[,12], digits, \(x, y) which(map_lgl(y, \(z) identical(x, z))))-1
tens <- map2_int(input[,13], digits, \(x, y) which(map_lgl(y, \(z) identical(x, z))))-1
ones <- map2_int(input[,14], digits, \(x, y) which(map_lgl(y, \(z) identical(x, z))))-1

sum(thou*1000 + hund*100 + tens*10 + ones)
input[2,11]
digits[[2]]
