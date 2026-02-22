library(tidyverse)

#############
#...........#
###D#A#C#C###
###D#A#B#B###
#############

# We need to minimise D movement, then C, then B, then A
# Clearly both A's need to move to the LEFT to make room
# for moving the B out of the D slot:
# this takes 5+5 = 10 energy:

#############
#AA.........#
###D# #C#C###
###D# #B#B###
#############

# Next we need to move the B and C out of the C slot:
# C moves out with 2 moves (200 energy)
#############
#AA.....C...#
###D# # #C###
###D# #B#B###
#############

# B then moves into it's correct slot with 60 energy:

#############
#AA.....C...#
###D# # #C###
###D#B# #B###
#############

# Both C's move: 300+400

#############
#AA.........#
###D# #C# ###
###D#B#C#B###
#############

# B moves: 70

#############
#AA.........#
###D#B#C# ###
###D#B#C# ###
#############

# Both D's move: 9 moves each: 18000 energy

#############
#AA.........#
### #B#C#D###
### #B#C#D###
#############

# Both A's move: 3 moves each: 6 energy

#############
#...........#
###A#B#C#D###
###A#B#C#D###
#############

# TOTAL: A: 10+6=16, B: 80+50=130, C: 200+300+400=900, D: 18000

18000+900+130+16

# Part 2: Now shit gets real: Pattern is now:

#############
#...........#
###D#A#C#C###
###D#C#B#A###
###D#B#A#C###
###D#A#B#B###
#############

# OK, so pretty clear we kinda need to do the same thing.
# We need to move the D's out LAST. If we can, always move
# a C INTO THE RIGHT PLACE FIRST.
# To do this we first need to get the B's and A's out of the
# C column. First move is easy: Move the A out of the way

# How would we code this up? Basically we can choose
# which of the pieces to move, and where to move it to.
# It's very similar to tower of hanoi.

# I THINK the answer is to clear the C column first?
# but I note that to do this we need 4 positions to
# the left and we only have 3. i.e. we could move the C to
# the right and the B,B to the left(?) then drop the C back
# in. BUT, then we can't move the C in the B column out
# until the A is moved there.

# So perhaps we move the A out to the right, then we do the B/A/B
# then we move the two C's in place:

# OK, so our moves are only: FROM a room into the hallway OR
# FROM the hallway into a room. NOTHING ELSE IS ALLOWED

# THis is a fun problem!:)

# Procedure is: Move A and C from 2 to the end? 7 + 700

#############
#.........CA#
###D# #C#C###
###D# #B#A###
###D#B#A#C###
###D#A#B#B###
#############

# Move B to the right and A to the left: 40+8

#############
#A....B...CA#
###D# #C#C###
###D# #B#A###
###D# #A#C###
###D# #B#B###
#############

# Drop B in. Move C out to right: 50+200

#############
#A.... .C.CA#
###D# # #C###
###D# #B#A###
###D# #A#C###
###D#B#B#B###
#############

# Move BAB: 70+8+80

#############
#AA... .C.CA#
###D# # #C###
###D#B# #A###
###D#B# #C###
###D#B# #B###
#############

# Move CCC:  500+600+500

#############
#AA... . . A#
###D# # # ###
###D#B#C#A###
###D#B#C#C###
###D#B#C#B###
#############

# Move ACB: 3+600+90

#############
#AA... . .AA#
###D#B#C# ###
###D#B#C# ###
###D#B#C# ###
###D#B#C# ###
#############

# Move DDDD then AAAA: 44000+5+5+9+9

# TOTAL:
(1000*11*4 + 2*5+2*9) + 3+600+90 + 500+600+500 + 70+8+80 + 50+200 + 40+8 + 7 + 700

# 47484

# OK, Now, how can we solve this with CODE.

# The moves are either OUT or IN. Nothing else matters.
# There are 7 positions in the hallway and 4 doors
# Can only move from hallway to a door. For each particular
# one there's only one choice of hallway.

# So each branch you need to consider which of the 11 nodes to
# move. Many of them will only have one destination (the hallway nodes)
# Clearly if you have a hallway node you can move, you must move it.
# If you don't have a hallway node you can move, then you must
# move from one of the 4 nodes, so each branch is really 4 choices,
# And your options for the 4 moves are then pretty small generally
# First move is 7 options. Second move is going to reduce quite a bit
# from there.

# It's a complete bipartite graph. You can move from any hallway
# location to any door. You can move from any door to any hallway
# so our data structure is a vector for the doors of 1,2,3,4
# and a vector for the hallway.

hallway <- rep(0,11) # not all things are filled!
doors <- list(c(4,4), c(1,1), c(3,2), c(3,2))
doors <- list(c(4,4,4,4), c(1,3,2,1), c(3,2,1,2), c(3,1,3,2))
door_len <- unique(lengths(doors))

move_cost <- c(1, 10, 100, 1000)

func_calls <- 0

least_cost <- function(hallway, doors) {
  # check for satisfied criteria
  complete_doors <- map2_lgl(doors, seq_along(doors), \(x, y) sum(x == y) == door_len)
  if (all(complete_doors)) {
#    cat("reached end!")
    return(0) # done!
  }

  # CHECK IF THIS STATE IS EVEN FEASIBLE.
  # We can cheaply compute the minimum cost from here
  # which is the cost of moving door D to the right spot?

  # check if we have any doors available: if we do, move an item in
  doors_available <- map2_lgl(doors,seq_along(doors), \(x, y) sum(x != y) == 0)
  wch_doors_available <- which(doors_available & !complete_doors)
  func_calls <<- func_calls+1
  if (func_calls %% 10000 == 0) {
    cat("done", func_calls, ":", hallway, ":", doors[[1]], "|", doors[[2]], "|", doors[[3]], "|", doors[[4]], "\n")
  }
  move_ins <- queue()
  for (d in wch_doors_available) {
    # door d is available: move a hallway item in if we can.
    # for door d, order doesn't matter: just move whichever one we can.
    # move out from the door in either direction and pick the first we reach
    left <- d*2; right <- (d+1)*2;
    while (TRUE) {
      # check if either left or right contain d
      if (hallway[left] == d) {
        move_ins$push(list(from=left, door=d))
        break
      }
      if (hallway[right] == d) {
        move_ins$push(list(from=right, door=d))
        break
      }
      if (left > 3 && hallway[left] == 0) {
        # can move more left
        left <- left-2
      } else if (left > 1 && hallway[left] == 0) {
        left <- left-1
      } else if (right < length(hallway)-2 && hallway[right] == 0) {
        right <- right+2
      } else if (right < length(hallway) && hallway[right] == 0) {
        right <- right+1
      } else {
        break # can't move
      }
    }
  }
  if (move_ins$size() > 0) { # have a door available and can move: always do so
    # branch on moving in
  #  cat("have", move_ins$size(), "move ins available\n")
    min_cost <- Inf
    while (move_ins$size() > 0) {
      move <- move_ins$pop()
      # compute our cost
      num_moves <- abs(move$from - (2*move$door+1))+(door_len-length(doors[[move$door]]))
      cost <- num_moves*move_cost[move$door]
      # make the move
      next_doors <- doors
      next_doors[[move$door]] <- c(move$door, doors[[move$door]])
      next_hall <- hallway
      next_hall[move$from] <- 0
 #     cat("moving from", move$from, "to", move$door, ":", hallway, "-", next_hall, ":", doors[[move$door]], "-", next_doors[[move$door]], "cost", cost, "\n")
      next_cost <- cost + least_cost(next_hall, next_doors)
      if (next_cost < min_cost) {
        min_cost <- next_cost
      }
  #    cat("done that move, num left is", move_ins$size(), "\n")
    }
    return(min_cost)
  }
  # OK, no doors are available for moves, so we must move OUT from a door into the hall
  # check if we can do this. If we can't, return early.
  move_outs <- queue()
  # ignore any doors that are available for moving into (or complete?)
  wch_doors <- which(!doors_available & !complete_doors)
  for (d in wch_doors) {
    # move from d if we can
    left <- d*2; right <- (d+1)*2;
    while(TRUE) {
      # can we move into either spot?
      if (hallway[left] == 0) {
        # can move left, so try it
        move_outs$push(list(door=d, to=left))
      }
      if (hallway[right] == 0) {
        # can move right, so try it
        move_outs$push(list(door=d, to=right))
      }
      if (left > 3 && hallway[left] == 0) {
        # can move more left
        left <- left-2
      } else if (left > 1 && hallway[left] == 0) {
        left <- left-1
      } else if (right < length(hallway)-2 && hallway[right] == 0) {
        right <- right+2
      } else if (right < length(hallway) && hallway[right] == 0) {
        right <- right+1
      } else {
        break # can't move
      }
    }
  }
  # do the moves (ideally this would be BFS rather than DFS?)
  min_cost <- Inf
  while (move_outs$size() > 0) {
    move <- move_outs$pop()
    # compute our cost
    num_moves <- abs(move$to - (2*move$door+1)) + 1 + (door_len - length(doors[[move$door]]))
    item_moved <- doors[[move$door]][1] # top of door
    cost <- num_moves*move_cost[item_moved]
    # make the move
    next_doors <- doors
    next_doors[[move$door]] <- doors[[move$door]][-1] # could use stack instead?
    next_hall <- hallway
    next_hall[move$to] <- item_moved
 #   cat("moving from", move$door, "to", move$to, ":", hallway, "-", next_hall, ":", doors[[move$door]], "-", next_doors[[move$door]], "cost", cost, "\n")
    next_cost <- cost + least_cost(next_hall, next_doors)
    if (next_cost < min_cost) {
      min_cost <- next_cost
    }
  }
  return(min_cost)
}

library(memoise)
least_cost <- memoise(least_cost)
ans <- least_cost(hallway, doors) #19046  YAY!
