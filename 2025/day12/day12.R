input <- "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2" |>
  str_split("\n") |> unlist()

input <- readLines("2025/day12/input.txt") |>
  str_split("\n") |> unlist()

# divide up our input
gaps <- input == ""
all <- tibble(wch = cumsum(gaps), input=input)

pieces <- all |> filter(wch < max(wch), input != "", !str_detect(input, ":")) |>
  group_by(wch) |> nest(data=input) |>
  pull(data)

areas <- all |> filter(wch == max(wch), input != "") |>
  extract(input, into=c('w', 'h', 'n'), regex="([0-9]+)x([0-9]+): (.*)", convert=TRUE) |>
  mutate(n = str_split(n, " ")) |>
  mutate(n = map(n, as.integer))

# ok, now we need to fit the pieces into each area
# this is basically a tetris solver or something?

# pieces can translate and rotate

# our pieces are at most 3x3. But in the final thing we have LOTS of them
# to fit into our (large) areas

# for each of the pieces, we have it's location (1..width-2, 1..height-2)
# and rotation (4)
# we then need to check if we can fit those pieces into the given space
# it could be a linear programming problem? There's constraints on
# location of each piece?

# or do we look at doing it by looking at the pieces and seeing how they
# fit together?

# 4 packs really well.
# 0 packs with a rotation into
# .###
# ####
# ####
# ###.
#
# 1 packs pretty well
#
# .##
# ###
# ###
# ##.
#

# 2 can be used to fill in holes?
# otherwise it doesn't pack that well
# .### same packing as 0
# ####
# ####
# ###.

# 3 isn't ideal?
#
# #.#.
# ###.
# ####
# .###
# .#.#
#
# 4 is great: can pack in real tight
#
# ####
# ####
# ####
#
# 5 isn't great?
# ##.
# .##
# ###

# ###..
# ####.
# .####
# ..###

# but two 5s and 4s are good:

# .AAACCC
# CAABBCC
# CCAABBC
# CCCBBB.

# there are some 600 unknown positions and rotations
# so something like 2000 unknowns.
# the constraints are pretty easy
# the rotation constraints could be done easier?
# 0 has 4 rotations
# 1 has 8 rotations
# 2 has 8 rotations
# 3 has 4 rotations
# 4 has 4 rotations
# 5 has 8 rotations
# so we could I guess have an extra set of
# constraints on the breakdown of those?
# constraints on position is easy.
# constraints on number of each part is easy.
# constraints on overlap are easy I think?
# need position of each item.
# chuck everything into gigantic LP solver.
# just need to know if there is a solution.
# don't care about optimality?!? Hmm, this
# will add issues?

# the sizes are so large that backtracking doesn't
# seem possible?!?

# perhaps it is really easy then?


# 0:
###
..#
###

# 1:
.##
##.
#..

# 2:
###
.##
.##

# 3:
  #.#
  ###
  #.#

# 4:
  #..
  ##.
  ###

# 5:
  ##.
  .##
  ###

# ok, for each piece, find the minimum and maximum area needed
# they all fit in a 3x3 so that's our max
sizes <- pieces |>
  map(\(x) x |> pull(input) |> str_split_fixed('', n=3)) |>
  map_int(\(x) sum(x == "#"))

areas |>
  mutate(total = map_int(n, \(x) sum(sizes*x))) |>
  mutate(tb = map_int(n, sum)) |>
  mutate(area = w*h) |>
  mutate(w_mod3 = (w %/% 3),
         h_mod3 = (h %/% 3),
         area_mod3 = w_mod3*h_mod3) |>
  filter(total < area) |>
  mutate(area_mod3 >= tb) # yay!
