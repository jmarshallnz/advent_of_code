library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- "19, 13, 30 @ -2,  1, -2
18, 19, 22 @ -1, -1, -2
20, 25, 34 @ -2, -2, -4
12, 31, 28 @ -1, -2, -1
20, 19, 15 @  1, -5, -3" |>
  str_split("\n") |> unlist() |>
  str_split_fixed(" ?[,@] ", n=6) |>
  apply(2, as.numeric)

input <- readLines("2023/day24/input.txt") |>
  str_split_fixed(" ?[,@] ", n=6) |>
  apply(2, as.numeric)

# ok, just a bunch of linear equations
# to 'solve'

# we start with just x and y and
# see if they intersect and when
v <- input[,4:5]
p <- input[,1:2]

# ok for pairs v, p solve p1 + v1*t1 = p2 + v2*t2 for some pair t1, t2
# solution is pretty simple: (v1-v2)*t = (p1-p2)

# e.g. (19,13) + (-2,1)t1 = (18,19)+(-1,-1)t2
# means 19 - 2 t1 = 18 - t2
#       13 + 1 t1 = 19 - t2
#.      -2 t1 + t2 = 18 - 19
#        1 t1 + t2 = 19 - 13
# solve(t(v) - t(p))
v <- v[1:2,]
p <- p[1:2,]

test_pair <- function(i, j) {
  m <- cbind(v[i,], -v[j,])
  b <- matrix(p[i,] - p[j,], ncol=1)
  # test for singularity
  detM <- m[1,1]*m[2,2] - m[1,2]*m[2,1]
  if (detM == 0)
    return(matrix(-Inf, nrow=2, ncol=2))
  t <- solve(m, -b)
  if (any(t < 0))
    return(matrix(-Inf, nrow=2, ncol=2))
  xy <- matrix(c(p[i,] + v[i,]*floor(t[1]),
                 p[i,] + v[i,]*t[1],
                 p[i,] + v[i,]*ceiling(t[1])), nrow=3, byrow=TRUE)
  return(xy)
}


dp <- function(i, j) {
  b <- matrix(p[i,] - p[j,], ncol=1)
}

pairs <- expand_grid(i=1:nrow(v), j=2:nrow(v)) |>
  filter(i < j) |>
  #filter(row_number() == 41819)
  mutate(xy = map2(i, j, test_pair)) |>
  mutate(inside = map(xy, \(x) apply(x, 1, \(y) all(y >= 200000000000000 & y <= 400000000000000)))) |>
  mutate(num_inside = map_dbl(inside, sum))

# ok, part 2 is the same but we're playing with an
# additional solve.

# we don't care about pairs above anymore, what we care about
# is finding a new one that collides with ALL of the above

# it's clear their collision needs to be in the future,
# but unclear if the collision has to be within some
# area, so let's assume the latter condition isn't needed.

# intriguing... Not sure I have any idea off the top of my head! :)

# I guess we have to find the rotation such that
# when the lines are projected down to x,y every single
# one of them intersects at the same location??

# first thought is if we have any parallel lines: if
# lines are parallel then any line that intersects
# both must lie in a plane. If they're parallel then
# their velocities must be the same (or a multiple
# of each other?)

# the same is easy to check:
dm <- input[,4:6] |> dist() |> as.matrix()
min(dm + diag(1, nrow=nrow(dm), ncol=ncol(dm))) # min 1

# multiples is harder? Essentially v1 = k*v2 for some k
# I guess that means if we row-reduce them we get 1?

# ok, so no parallel lines, and none that intersect
# that means no obvious shortcuts

qr(input[1:2,4:6])

v <- input[,4:6]
check_rank <- function(i, j) {
  qr(v[c(i,j),,drop=FALSE])$rank
}

# hmm, if we know we have some intersecting in x and y

# how can we make use of part A?
# part A was about when x,y intersected on the existing
# lines. How does that help?
# for a NEW line (which we don't know) we can easily
# apply to check if it hits them all as to hit
# in 3D it must at least hit in (x,y) as well.
apply(v, 2, min)
apply(v, 2, max) # velocities as large as 829 and as small as -745

is.prime <- function(n) n == 2L || all(n %% 2L:max(2,floor(sqrt(n))) != 0)

z <- v[,3]
which(z %in% c(613,829))
pz = p[which(z == 613),3]
pz %% 613 # this one is always 162 mod 613

pz = p[which(z == 829),3]
pz %% 829 # this one is always 403 mod 829

# need the number to be 162 mod 613 AND 403 mod 829...

# so the number is 403 + k * 829
# and              162 + (k+l) * 613 for some k, l integers
#

# Hmm, this is getting nowhere fast.

# what about this: we know at time ZERO where they all are and
# we're they're headed: this defines a convex hull
# which constrains each intersection point. The start point
# is outside that convex hull (by 1 time unit).
# so this restricts where p is, but probably not much as it's
# only masking off a very small part of space, leaving all
# of it on the outside...

# maybe the number of x,y isn't that many?
# so we can do lots of possibles?
v_xy <- expand_grid(v1 = min(v[,1]):max(v[,1]), v2 = min(v[,2]):max(v[,2]))

# ok, that's a lot... also unclear how it helps?

# TBH I think the rotate and project idea seems the sound-est?
# given a vector V defining the rotation matrix,
# project everything down onto the plane and check
# intersection point.
# V (final line direction) gives us the normal to the plane
# that we're going to project on.
# we then project our vectors onto the plane. This is p - (n ⋅ (p - o)) * n where o is origin of plane, p point
# n normal. # PROBLEM: we don't know the origin! But maybe we don't care as we can just use
# the origins in the original domain? (NOT SURE?!)

# NO IDEA HOW TO DO THIS ONE without some sort of weird iteration thing



# find a position p such that it intersects with the above?!?





pz[1] %% 613
pz[2] %% 829

z[map_lgl(abs(z), is.prime)] |> unique() |> sort() # prime z's
# so all points on the line have the same value for  (pz mod z)
# e.g. (pz1 mod 613) and (pz2 mod 829)

v[,3] |> sort()

library(pracma)
how_close <- function(i, j) {
  n = cross(v[i,], v[j,])
#  |𝐧⋅(𝐫1−𝐫2)|‖𝐧‖
  abs(sum(n * (p[i,] - p[j, ])))/sqrt(sum(n^2))
}
pairs <- expand_grid(i=1:nrow(v), j=2:nrow(v)) |>
  filter(i < j) |>
  mutate(r = map2_int(i, j, check_rank)) |>
  mutate(d = map2_dbl(i, j, how_close))

# the lines are very far apart, so

  count(r) # all rank 2 so no nice things to be had there

# the other option is the intersect. If they intersect then
# they lie in a plane.

# to figure this out, we could find the minimum distance between
# the lines?



pairs <- expand_grid(i=1:nrow(v), j=2:nrow(v)) |>
  filter(i < j) |>
  #filter(row_number() == 41819)
  mutate(xy = map2(i, j, test_pair)) |>
  mutate(inside = map(xy, \(x) apply(x, 1, \(y) all(y >= 200000000000000 & y <= 400000000000000)))) |>
  mutate(num_inside = map_dbl(inside, sum))

pairs |>
  unnest(inside) |> summarise(sum(inside))

  mutate(d = map_dbl(xy, \(x) sum(abs(x*0.000000000001))))

ggplot(pairs |> filter(d > 199 & d < 202)) + aes(d) + geom_histogram()

  #unnest(dp)


pairs |>
  summarise(sum(inside)) # still 9789?

# large numbers mean condition is going to be hard, and we're possibly going
# to get a few wrong due to rounding.
# I guess the soultion

# an alternate perhaps is to consider where the lines intersect the area?
# we can rule out the parallel lines pretty quickly.
# then if they're not parallel we know they intersect.
# question is where they intersect, right?
# we can easily work out where the lines intersect the boundary of our
# region I think?

p = input[,1]
v = input[,4]

# ok, so need to find a direction w and position p so that...
# p = 24 and v = -3...

# we know p >= 20 and v <= 0 OR p <
# keep taking steps until modulo V (unknown) they're all the same?
for (V in 2:5) {
  pi = p
  for (i in 1:10) {
    pi = pi + v
    if (length(unique(pi %% V)) == 1) {
      cat("matches with V=", V, "i=", i, "\n")
    }
  }
}

#p1 + v1*t1 = P + V*t1 (2 constraints, 2+2+1, unknowns)
#p2 + v2*t2 = P + V*t2 (4 constraints, 2+2+2 unknowns)
#p3 + v3*t3 = P + V*t3 (6 constraints, 2+2+3 unknowns)
#p4 + v4*t4 = P + V*t4 (8 constraints, 2+2+4 unknowns) # solvable??!
#p5 + v5*t5 = P + V*t5 (10 constraints, 2+2+5 unknowns) # hmm...
# with all pairs you can remove t1..t5, right?

# and from there you have quad equations but no diff between them
# so can eliminate the quadratic terms and solve
v1*t1 - V*t11 = P-p1
v2*t2 - V*t12 = P-p2
v3*t3 - V*t13 = P-p3
v4*t4 - V*t14 = P-p4
v5*t5 - V*t15 = P-p5

# this looks like a quadratic problem?
# set t11=0 drops the first down to v1*t1 = P-p1 -> P = p1 + v1*t1
# so constrains P to be on the first line
v2[1]*t2 - v1[1]*t12 - V1*t12 = p1[1] - p2[1]
v3[1]*t3 - v1[1]*t13 - V1*t13 = p1[1] - p3[1]
v4[1]*t4 - v1[1]*t14 - V1*t14 = p1[1] - p4[1]
v5[1]*t5 - v1[1]*t15 - V1*t15 = p1[1] - p5[1]

v2[2]*t2 - V2*t12 = p2[1] + v2[1]*t2 - p2[2]
v3[2]*t3 - V2*t13 = p2[1] + v2[1]*t2 - p3[2]
v4[2]*t4 - V2*t14 = p2[1] + v2[1]*t2 - p4[2]
v5[2]*t5 - V2*t15 = p2[1] + v2[1]*t2 - p5[2]

v2[3]*t2 - V3*t12 = p2[1] + v2[1]*t2 - p2[3]
v3[3]*t3 - V3*t13 = p2[1] + v2[1]*t2 - p3[3]
v4[3]*t4 - V3*t14 = p2[1] + v2[1]*t2 - p4[3]
v5[3]*t5 - V3*t15 = p2[1] + v2[1]*t2 - p5[3]

# we can't really eliminate V1 at all without dividing by t12 etc?

# MUST be a linear solution to this thing
# for two hailstones and a rock (4 unknowns) and four times:
vx*t1 + px = vrx*t1 + prx # 2 equations with 5 unknowns (t1, rock)
vy*t1 + py = vry*t1 + pry
# the key insight is the rock and first hailstone meet at time 1.

# we can use this to eliminate t1 altogether and get
# a linear system
(vx-vrx)*t1 + px = prx
(vy-vry)*t1 + py = pry
(vz-vrz)*t1 + pz = prz

(vy-vry)*(vx-vrx)*t1 + (vy-vry)*px = (vy-vry)*prx # x and y
(vx-vrx)*(vy-vry)*t1 + (vx-vrx)*py = (vx-vrx)*pry

(vz-vrz)*(vx-vrx)*t1 + (vz-vrz)*px = (vz-vrz)*prx # x and z
(vx-vrx)*(vz-vrz)*t1 + (vx-vrx)*pz = (vz-vrz)*prz

(vy-vry)*px - (vx-vrx)*py = (vy-vry)*prx - (vx-vrx)*pry
(vz-vrz)*px - (vx-vrx)*pz = (vz-vrz)*prx - (vx-vrx)*prz

# ok, now our unknowns are vry, vrz, prx and prz # 4 unknowns linear system

# similarly, stone K and rock also meet at the same time (different to t1), so the exact
# same system applies to all the stones

# so we can pick any pair of stones and get the same set of equations

# for stone 1:
vy1*px1-vry*px1 - vx1*py+vrx*py = vy1*prx-vry*prx - vx1*pry + vrx*pry
vz1*px1-vrz*px1 - vx1*pz+vrx*pz = vz1*prx-vrz*prx - vx1*prz + vrx*prz

# note we have some quadratic terms on the right:
vy1*px1 - vx1*py1 = vry*px1 - vrx*py1 + vy1*prx - vx1*pry + vrx*pry-vry*prx
vz1*px1 - vx1*pz1 = vrz*px1 - vrx*pz1 + vz1*prx - vx1*prz + vrx*prz-vrz*prx

# we can get rid of them by pairing with another rock:
vy2*px2 - vx2*py2 = vry*px2 - vrx*py2 + vy2*prx - vx2*pry + vrx*pry-vry*prx
vz2*px2 - vx2*pz2 = vrz*px2 - vrx*pz2 + vz2*prx - vx2*prz + vrx*prz-vrz*prx

(vy1*px1 - vx1*py1) - (vy2*px2 - vx2*py2) = vry*px1 - vrx*py1 - (vry*px2 - vrx*py2) + vy1*prx - vx1*pry - (vy2*prx - vx2*pry)
                                          = vry*(px1-px2) - vrx*(py1-py2) + prx*(vy1-vy2) - pry*(vx1-vx2)
px1*(vy1-vy2)

pxr*(vy0 - vyN) + pyr*(vxN - vx0) + vxr*(pyN - py0) + vyr*(px0 - pxN) = px0*vy0 - py0*vx0 - pxN*vyN + pyN*vxN

# this is a linear system in 4 variables using 2 hailstones

# we then repeat using another pair of stones
p <- input[,1:3]
v <- input[,4:6]

get_system <- function(i, j) {
  vi = v[i,]; vj = v[j,]; pi = p[i,]; pj = p[j,]
  # ok, now generate our system
  rhs = (vi[2]*pi[1] - vi[1]*pi[2]) - (vj[2]*pj[1] - vj[1]*pj[2])
  coef = c(pj[2]-pi[2], pi[1]-pj[1], vi[2]-vj[2], vj[1]-vi[1])
  c(coef, rhs)
}

m <- rbind(get_system(1, 2),
      get_system(1, 3),
      get_system(1, 4),
      get_system(1, 5))

rock_xy <- solve(m[,1:4], m[,5]) # ok, that's our vx, vy, px, py
# we can then use the first stone to figure out pz, vz by solving
# for t
t1 <- (rock_xy[3]-p[1,1])/(v[1,1]-rock_xy[1]) # 5
t2 <- (rock_xy[3]-p[2,1])/(v[2,1]-rock_xy[1]) # 5
#v[1,3]*t1 + p[1,3] = prz - t1*vrz
#v[2,3]*t2 + p[2,3] = prz - t2*vrz

vrz <- (v[1,3]*t1-v[2,3]*t2+p[1,3] - p[2,3])/(t2 - t1)
# and finally
prz <- v[1,3]*t1 + p[1,3] + t1*vrz

c(rock_xy[3:4],prz) |> sum()

# ok, now we can solve directly

