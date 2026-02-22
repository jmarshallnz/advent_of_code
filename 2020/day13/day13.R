library(tidyverse)

options(digits = 22,
        pillar.max_dec_width = 22)

# classic primes..
input <- readLines("2020/day13/input.txt")

min_depart <- input[1] |> as.numeric()
bus_list <- input[2] |> str_split(',') |> unlist() |> as.numeric()
bus_list <- '7,13,x,x,59,x,31,19'  |> str_split(',') |> unlist() |> as.numeric()
busses <- bus_list[!is.na(bus_list)]

wait_times = busses - (min_depart %% busses)
wch <- which.min(wait_times)
wait_times[wch]*busses[wch]

# part 2: Find the time t such that bus 1 departs at time t, bus 2 at time t+1 etc.
bus_offsets <- (busses - (which(!is.na(bus_list))-1) %% busses) %% busses

# ok, so bus 19 goes at time t. So t %% 19 = 0.
# bus 41 goes at time 9. So t %% 41 = 9
# bus 523 goes at time 19. So t %% 523 = 19

# All these numbers are _coprime_
# This is a system of linear equations, right?

# Take just the first two equations.
# we know that: 0*41,1*41,2*41..,18*41 mod 19 = 0,1,2,..18 mod 19 (i.e. no double-ups, so unique solution)
# so here we have t = k2*41+9
# in mod 19 this has to be 0.
# so k2*41 = 9 %% 19, and k2 can only be a single number (modulo 19)

# t %% (19*41) = 9?
# t = k2*41 + 9
# We want to know which of 0..((19*41)-1) is 0 mod 19 and 9 mod 41
which((0:(19*41-1)) %% 19 == 0 &
      (0:(19*41-1)) %% 41 == 9)

# 665 is the smallest that satisfies the above though, right?
# how we can we work out 665 directly? Can we?
# 665 %/% 41 # 16 hmm
# 665 = 41*16 + 9 # Hmm, not sure where the 16 comes from. (0:18)[which((41*(0:18)) %% 19 == 9)] 3??
# 665 = 19*35 # 19 * (0:40)[which((19*(0:40)) %% 41 == 9)]

# What about the first two? We know 665 is the first. What about the second? I guess we'd need another
(665 + 41*19*0) %% 19 == 0
(665 + 41*19*5) %% 41 == 9

# So now have: how do we get 665?
t %% 779 = 665
# also require:
t %% 523 = 19
# so t %% (779*523) = ??
(142 + (779*523)) %% 41 # hmm, 19?
(142 + (779*523)) %% 19 # hmm, 9?

# t %% (19*41) = 665
# t %% 523 = 19
# We need to solve both of these
# t = k1*(19*41)+665 # for integer k1
# t = k2*(523)+19 # for integer k2

# so t = 19*41*523*k0 + 19*41*l0 + 665
# for some integers k0 and l0.
# so modulo 19*41*523

# l0 must be in 0..522
which((19*41*(0:522) + 665) %% (523) == 19) # 166 which is 165 as 0-based

(165*19*41 + 665 + 19*41*523) %% 19
(165*19*41 + 665 + 19*41*523) %% 41
(165*19*41 + 665 + 19*41*523) %% 523 # 19

# Next number is 165*19*41 + 665

# OK, what about the NEXT number
# t = 19*41*523*17*k0 + 19*41*523*l0 + 129200
# l0 must be in 0..16:
l0 = which((19*41*523*(0:16) + 129200) %% (17) == 2)-1 # 166
ox = (19*41*523*l0 + 129200)

ox %% 19
ox %% 41
ox %% 523
ox %% 17

smallest <- 0
for (i in 2:length(busses)) {
  # we have t = prod(busses[seq_len(i)])*k + prod(busses[seq_len(i-1)])*l + smallest
  # where smallest satisfies conditions through seq_len(i-1).
  # We need to also now satisfy condition i: t %% busses[i] == bus_offsets[i]
  # Hence (prod(busses[seq_len(i-1)])*l + smallest) %% busses[i] == bus_offsets[i]
  # due to busses[i] $ being coprime, we know l must be in 0..busses[i]-1:
  l_seq <- seq_len(busses[i])-1
  l <- which((prod(busses[seq_len(i-1)])*l_seq + smallest) %% busses[i] == bus_offsets[i])-1
  smallest <- prod(busses[seq_len(i-1)])*l + smallest
}

# Just adding a note here: Apparently this is the chinese remainder theorem!

