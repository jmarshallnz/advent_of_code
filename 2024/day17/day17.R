library(tidyverse)

instructions <- c('adv', 'bxl', 'bst', 'jnz', 'bxc','out','bdv','cdv')

# registers
A <- 1
B <- 2
C <- 3

combo <- function(reg, operand) {
  if (operand < 4) {
    return(operand)
  }
  reg[operand %% 4 + 1] # Potentially this is gonna fault...
}

# R's bitwXor is 32 bit, but we don't care about the top bits anyway
xor <- function(a, b) {
  bitwXor(a %% 2^31,b %% 2^31)
}

adv <- function(state, operand, store = A) {
  num <- state$reg[A]
  den <- 2 ^ combo(state$reg, operand)
  state$reg[store] <- num %/% den
  state
}

bxl <- function(state, operand) {
  state$reg[B] <- xor(state$reg[B], operand)
  state
}

bst <- function(state, operand) {
  state$reg[B] <- combo(state$reg, operand) %% 8
  state
}

jnz <- function(state, operand) {
  if (state$reg[A] == 0)
    return(state)
  state$ptr <- operand-2
  state
}

bxc <- function(state, operand) {
  state$reg[B] <- xor(state$reg[B], state$reg[C]) # could call bxl for this?
  state
}

out <- function(state, operand) {
  state$out <- c(state$out, combo(state$reg, operand) %% 8)
  state
}

bdv <- function(state, operand) {
  adv(state, operand, store=B)
}

cdv <- function(state, operand) {
  adv(state, operand, store=C)
}


# read input
input <- read.table(text="Register A: 729
Register B: 0
Register C: 0

Program: 0,1,5,4,3,0", sep=':')

input <- read.table("day17/input.txt", sep=":")

registers <- input$V2[1:3] |> as.integer()
program   <- input$V2[4] |> str_split(",") |> unlist() |> as.numeric()

state <- list(ptr = 0, reg = registers, out = integer())
# run the program
while(state$ptr < length(program)) {
  op=program[state$ptr+1]+1
  state = do.call(instructions[op], args=list(state, program[state$ptr+2]))
  state$ptr = state$ptr+2
}
paste(state$out, collapse=",")

# Part 2: Fun!

# We're going to have a very long run of the program, given our input
# to part 1 was already very high.

# let's do some analysis. By the looks the only jmp is right at the end
# and it just jumps to 0, so loops around. The operations are:
# op1: B = A %% 8
# op2: B = xor(B, 2)
# op3: C = A / 2^B
# op4: B = xor(B, C)
# op5: A = A / 8
# op6: B = xor(B, 7)
# op7: out = B %% 8
# op8: if(A) jump to op1

# so we see that only register A matters in each loop, and the only change
# to register A is the division by 8.

# thus, we can work backwards from our output: Whatever A is at the last
# iteration must be in 1..7 and must result in the last entry in program (0).

# Once we know the possibilities for A for the last character, the next one
# must be 8 times that plus 0..7 (can it be 0? Probably not but it won't hurt)

# Then we just repeat. There's potential that multiple values of A give
# the output we need. I think it would be wise to keep them (assuming it doesn't
# blow up exponentially) as the lower bits of A are being used in op C presumably,
# and plausibly it won't just be the lowest possible A from step k that is useful
# for step k+1?

run_program <- function(regA) {
  state <- list(ptr = 0, reg=c(regA, 0, 0), out=integer(0))
  while(state$ptr < length(program)) {
    op=program[state$ptr+1]+1
    state = do.call(instructions[op], args=list(state, program[state$ptr+2]))
    state$ptr = state$ptr+2
  }
  state$out
}

# accumulate our registerA value
regA <- 0
for (i in 0:15) {
  # run the program with 0:7 as input to A.
  # sometimes there'll be multiple A's and we'll need to append them
  regA = expand_grid(x=0:7, curr=regA) |>
    mutate(poss = curr*8 + x) |>
    mutate(out = map(poss, ~run_program(regA=.))) |>
    mutate(pass = map_lgl(out, \(x) all(x == program[(16-i):16]))) |>
    filter(pass) |>
    pull(poss)
}
min(regA)

# check:
run_program(regA=min(regA)) == program
