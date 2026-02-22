library(tidyverse)

# and now for some linear algebra...
# our scanners have a rotation/reflection matrix (24 options apparently)
# plus their x,y,z coords that are unknowns. This is 12 unknowns,
# with restrictions that they're integers, plus for the rotation matrix
# that they're all +/-1 or 0
# abs_beacon <- rot_i %*% rel_i + trans_i
# This is linear in the 12 unknowns. We won't ever know exact trans
# as we can always shift one, so set scanner 0 as the coordinate system
# i.e. rot_0 = I, trans_0 = 0
# We can solve for rot_1 noting that at least 12 beacons overlap,
# but we don't know which of them: some of them won't overlap as they'll
# be too far from the scanner.

# Now, there's only 24 options for rot_i, so we could just brute
# force all of them.

# After that, there's only 3 variables to solve for: assuming we
# have the direction correct we need to find a subset of 12
# from the first scanner (abs coords) that have the same offsets
# from each other as a subset of 12 from the second scanner.

# there's about 27 beacons that each scanner sees, and choosing
# subsets of 12 from that is not doable, so there must be a
# smarter way. Presumably the x,y,z coords all need to overlap
# perfectly, so can we solve it just in x/y/z separately?

# i.e. assume we have the rotation correct. Then we have
# x coordinates for two sets of 27 points, where one lot
# is just a constant offset of the others.
# so differences from x[1] to the other 26 will cancel that
# offset, and 12 of them should match.

# so procedure is: for each of 24 directions, transform
# the relative to absolute coords, and compute deltas
# then 12 deltas should match and we're done.

input <- '--- scanner 0 ---
404,-588,-901
528,-643,409
-838,591,734
390,-675,-793
-537,-823,-458
-485,-357,347
-345,-311,381
-661,-816,-575
-876,649,763
-618,-824,-621
553,345,-567
474,580,667
-447,-329,318
-584,868,-557
544,-627,-890
564,392,-477
455,729,728
-892,524,684
-689,845,-530
423,-701,434
7,-33,-71
630,319,-379
443,580,662
-789,900,-551
459,-707,401

--- scanner 1 ---
686,422,578
605,423,415
515,917,-361
-336,658,858
95,138,22
-476,619,847
-340,-569,-846
567,-361,727
-460,603,-452
669,-402,600
729,430,532
-500,-761,534
-322,571,750
-466,-666,-811
-429,-592,574
-355,545,-477
703,-491,-529
-328,-685,520
413,935,-424
-391,539,-444
586,-435,557
-364,-763,-893
807,-499,-711
755,-354,-619
553,889,-390

--- scanner 2 ---
649,640,665
682,-795,504
-784,533,-524
-644,584,-595
-588,-843,648
-30,6,44
-674,560,763
500,723,-460
609,671,-379
-555,-800,653
-675,-892,-343
697,-426,-610
578,704,681
493,664,-388
-671,-858,530
-667,343,800
571,-461,-707
-138,-166,112
-889,563,-600
646,-828,498
640,759,510
-630,509,768
-681,-892,-333
673,-379,-804
-742,-814,-386
577,-820,562

--- scanner 3 ---
-589,542,597
605,-692,669
-500,565,-823
-660,373,557
-458,-679,-417
-488,449,543
-626,468,-788
338,-750,-386
528,-832,-391
562,-778,733
-938,-730,414
543,643,-506
-524,371,-870
407,773,750
-104,29,83
378,-903,-323
-778,-728,485
426,699,580
-438,-605,-362
-469,-447,-387
509,732,623
647,635,-688
-868,-804,481
614,-800,639
595,780,-596

--- scanner 4 ---
727,592,562
-293,-554,779
441,611,-461
-714,465,-776
-743,427,-804
-660,-479,-426
832,-632,460
927,-485,-438
408,393,-506
466,436,-512
110,16,151
-258,-428,682
-393,719,612
-211,-452,876
808,-476,-593
-575,615,604
-485,667,467
-680,325,-822
-627,-443,-432
872,-547,-609
833,512,582
807,604,487
839,-516,451
891,-625,532
-652,-548,-490
30,-46,-14' |> str_split('\n') |> unlist()

input <- readLines('2021/day19/input.txt')
scanners <- tibble(input = input, gap = cumsum(input == "")) |>
  filter(!str_detect(input, 'scanner'), input != "") |>
  extract(input, into=c('x', 'y', 'z'), regex="([-0-9]+),([-0-9]+),([-0-9]+)", convert=TRUE) |>
  group_by(gap) |> group_split()

# we have 25 scanners, so lots of pairs to consider. We need to solve
# the set of equations for the scanners.

# OK, scanner 0 is absolute coordinates
# for each of the other scanners, find if they overlap with 0
# process is: take the 24 possible direction matrices
# choose which axis is fixed (other entries are 0)
#
vec_cross <- function(x, y) {
  c(x[2]*y[3] - x[3]*y[2],
         -(x[1]*y[3] - x[3]*y[1]),
         x[1]*y[2] - x[2]*y[1])
}
rots <- list()
for (i in 1:3) {
  for (j in c(-1, 1)) {
    x <- rep(0, 3)
    x[i] <- j*1
    for (k in setdiff(1:3, i)) {
      for (l in c(-1, 1)) {
        y <- rep(0, 3)
        y[k] <- l*1
        z <- vec_cross(x, y)
        rots[[length(rots)+1]] <- cbind(x, y, z)
      }
    }
  }
}

# OK, so we grab scanner 1 and we rotate all the observations by each of
# these matrices in turn. Then we compute all pair-wise distances to scanner 0
# and check for 12 point overlap.

# if it exists we transform to absolute coordinates and then use that
# second scanner to compare to others. Rinse and repeat

# OK, now need to do this progressively. We maintain absolute, compare and relative
# lists
absolute <- list(scanners[[1]])
compare  <- stack(scanners[1])
relative <- scanners[-1]

scanner_locs <- list(data.frame(tx=0, ty=0, tz=0, scanner=0))

while(length(relative) >= 0 && compare$size() > 0) {
  # grab the first compare and the first
  comp_abs <- compare$pop()
  next_relative <- list()
  # iterate through our relative list, expand and compare
  for (r in relative) {
    comp_rel <- map_dfr(rots, \(x) { r[,1:3] |> as.matrix() %*% x |> as.data.frame()}, .id='rot')
    overlap <- comp_rel |> cross_join(comp_abs |> select(x, y, z)) |>
      mutate(tx = x.x-x.y, ty = y.x - y.y, tz = z.x - z.y) |>
      count(rot, tx, ty, tz) |>
      filter(n >= 12)
    if (nrow(overlap) > 0) {
      # transform this scanner to absolute coordinates]
      next_abs <- comp_rel |> inner_join(overlap) |>
        mutate(x = x-tx, y = y-ty, z = z-tz) |>
        select(rot, x, y, z)
      # add to our absolute list, and also our compare stack
      absolute[[length(absolute)+1]] <- next_abs
      # also record scanner location
      scanner_locs[[length(scanner_locs)+1]] <- overlap |> mutate(scanner = first(r$gap))
      compare$push(next_abs)
    } else {
      # not done, add to our next relative list
      next_relative[[length(next_relative)+1]] <- r
    }
  }
  relative <- next_relative
}
absolute |> bind_rows(.id = 'scanner') |>
  select(x, y, z) |> unique() |>
  nrow()

# 79 on example as needed.

# Part 2 we need tx, ty, tz as well.
scanner_locs |> bind_rows() |>
  select(tx, ty, tz) |> dist(method='manhattan') |> max()
