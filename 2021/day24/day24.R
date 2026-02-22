library(tidyverse)

# ALU problem: Decode what the program is doing (disassembly basically)

# I note that after every inp statement there's a mul x,0 which is equivalent
# to x=0, thus, only y and z are unknowns for every input 'zone'. This simplifies
# things a bunch.

# Let's see if the subprogram is largely the same. It probably is!
input <- read.table('2021/day24/input.txt', sep=' ', fill = TRUE, header=FALSE)

subroutines <- input |>
  mutate(step=cumsum(V1 == "inp")) |>
  group_by(step) |> mutate(line = row_number())

subroutines |>
  group_by(line) |>
  summarise(v1=n_distinct(V1), v2=n_distinct(V2),v3=n_distinct(V3))

# OK, only v3 changes for 5 lines, so we have the same repeated subroutine
params <- subroutines |>
  filter(line %in% c(5,6,16)) |>
  select(-V1, -V2) |>
  pivot_wider(names_from=line, values_from=V3) |>
  ungroup() |>
  select(chk = `6`, add = `16`, div = `5`) |>
  mutate(across(everything(), as.integer))

# ok, now check if the instructions match across the board
submat <- subroutines |> select(-line) |>
  as.matrix()

equal <- apply(submat, 1, \(x) length(unique(x)) == 1)

submat[!equal,] # three parameters needed

# OK, so the subroutine is identical with the exception of three parameters
subroutine <- function(z, input, chk, add, div) {
  #inp w    ; w = input

  #mul x 0
  #add x z
  #mod x 26 ; x = z %% 26

  #div z 26 ; z = z %/% divZ

  #add x -3 ; x = x + offX

  #eql x w
  #eql x 0 ; x = ifelse(x != w, 1, 0) -> x = ifelse((z %% 26)+offX != w, 1, 0)

  # -> insert z division

  #mul y 0
  #add y 25
  #mul y x
  #add y 1 ; y = ifelse(x == 1, 26, 1)

  #mul z y ; z = z*ifelse(x == 1, 26, 1)

  #mul y 0
  #add y w
  #add y 12
  #mul y x ; y = ifelse(x == 1, input+offY, 0)

  #add z y ; z = z+y

  # NOTE: %div% here rounds TO ZERO rather than truncating.
  #       %mod% would then presumably allow negatives? i.e. %mod% X = X - X %div% X?

  if ((z %% 26) + chk != input) {
    z=(z %/% div)*26+input+add
  } else { # x=0 branch
    z=z %/% div
  }
  return(z) # only z matters
}

run_sub <- function(z, input, wch) {
  subroutine(z, input, chk=params$chk[wch],
             add=params$add[wch], div=params$div[wch])
}

# Noting that divZ is either 1 or 26, what this is doing is either
#
# 1. multiplying Z by 26 (and adding stuff) (steps 1,2,3,6,7,9,11). Coincides with offX in 11,12,14,15 which means it'll get hit everytime.
#
# 2. or dividing by 26 then multiplying by 26 (and adding stuff (input+offZ always less than 26)). This replaces existing Z with input+offZ.
# This happens in steps 4,5,8,10,12,13,14 and there's a bunch of inputs that would hit it as offX is negative for those.
#
# 3. or dividing Z by 1 (steps 1,2,3,6,7,9,11 when z %mod% 26) + offX == input. But, offX in 11,12,14,15 so never gets hit unless Z is negative?
# But Z can never be negative as offZ is always postive, so this branch doesn't happen.
#
# 4. OR dividing Z by 26 (steps 4,5,8,10,12,13,14) when one specific input will hit (Z %mod% 26) + offX == input.

# As Z starts at 0 and needs to end at zero, the number of times we multiply by 26 has to be the same as the number we divide.
# As step 1 always is the case for 7 steps, it must be step 4 for the others.
# This then defines our input exactly for those 7 steps, right?

# hmm, try a recursive call I guess?
param_list <- params |> rowwise() |> group_split() |> map(as.list)

valid <- list()

recurse_solve <- function(z, input, params) {
  # pop the top off our parameters
  if (length(params) == 0) {
    if (z == 0) {
      valid[[length(valid)+1]] <<- input
    }
    return(z == 0)
  }
  param <- params[[1]]
  next_params <- params[-1]
  if (param$div == 26) { # solve step
    i = (z %% 26) + param$chk
    if (i < 1 || i > 9) {
      return(FALSE)
    }
    # update z and recurse in
    nextz <- subroutine(z, i, chk=param$chk, add=param$add, div=param$div)
    return(recurse_solve(nextz, input=c(input, i), next_params))
  } else { # run step
    solved <- FALSE
    for (i in 1:9) {
      # update z and recurse in
      nextz <- subroutine(z, i, chk=param$chk, add=param$add, div=param$div)
      if (recurse_solve(nextz, input=c(input, i), next_params))
        solved <- TRUE
    }
    return(solved)
  }
}

ans <- recurse_solve(z=0, input=c(), params = param_list)

# part 1:
map_dbl(valid, \(x) as.numeric(paste0(x, collapse=''))) |> max()

# part 2:
map_dbl(valid, \(x) as.numeric(paste0(x, collapse=''))) |> min()
