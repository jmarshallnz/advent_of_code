library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

input <- read.csv("2025/day09/input.txt", header=FALSE)

# ok, biggest rectangle is every possible combination thereof
input |> cross_join(input) |>
  mutate(area = abs(V1.y - V1.x + 1)*abs(V2.y - V2.x + 1)) |>
  slice_max(area)

# neat twist for part 2: the input defines
# the total area we can be in via a long 'circuit'
# through the area.
# our rectangles (which are really large!!) must
# be made completely inside the middle of that area.
# and the area is concave, not convex.
# BUT, the rectangles are convex, so presumably we
# need some sort of "maximal convex shape inside
# a concave shape" type algorithm?

# let's start off with a plot
ggplot(input) +
  aes(x=V1, y=V2) +
  geom_path()

# hah, it's just a circle with a big cutout :)
ggplot(input) +
  aes(x=V1, y=V2, col = V2 >= 48000 & V2 <= 50100) +
  geom_path()

# OK, so the cutout is defined by only a few vertices:
input |> tibble::rowid_to_column() |>
  filter(V2 >= 48000 & V2 <= 50100)

# ok, the cutout is really made up of the vertices
# 249 and 250:

input |> tibble::rowid_to_column() |>
  ggplot() +
  aes(x=V1, y=V2, col = rowid %in% 249:250) +
  geom_point()

# the largest rectangles must clearly use
# one of those dots which are:
input[249:250,]

# so x (V1) is 94967 in both cases.
# let's find the biggest rectangle above and below:
input |>
  filter(V1 >= 94967, V2 > 50085) |>
  arrange(desc(V2)) # ok, we can go as high as 68035 upwards

input |>
  filter(V1 < 50000, V2 <= 68035, V2 > 50085) |>
  arrange(desc(V2)) # ok, has to be 5633, 67624

#area:
(94967-5633+1)*(67624-50085+1)

# could also go below:
input |>
  filter(V1 >= 94967, V2 < 48699) |>
  arrange(V2) # ok, we can go as low as 33295

input |>
  filter(V1 < 50000, V2 >= 33295, V2 < 48699) |>
  arrange(V2) # ok, has to be 3240, 33344

#area:
(94967-3240+1)*(48699 - 33295 + 1) # smaller, so one above

# Plus I think this one isn't valid anyway as it cuts back in to 4251...
input

# Alternate method:

ggplot(input) +
  geom_rect(data=max1, mapping=aes(xmin=V1.x, ymin=V2.x, xmax=V1.y, ymax=V2.y), fill='red') +
  geom_path(aes(x=V1, y=V2)) # looks good


# I think? # Hmm, need we only check that our rectangle edges
# cross the "lines" of our X/DX? How many checks are they?
# about 400k pixels for rectangle edges, and about the same
# our rectangles are convex, so we can easily check if points
# are inside or outside them. So we can easily determine if
# line segments intersect them. As all the line segments
# are horizontal/vertical, I think if any point is inside by
# more than 1 pixel then it's a dud?

# we have to do top and bottom separately for this:

pt_in_rect <- function(rect, x, y) {
  x1 <- min(rect[1], rect[3]); x2 <- max(rect[1], rect[3])
  y1 <- min(rect[c(2,4)]); y2 <- max(rect[c(2,4)])
  return(any(x > x1 & x < x2 & y > y1 & y < y2))
}

rects1 <- input[1:249,] |> cross_join(input[1:249,])
in_out1 <- rects1 |> tibble::rowid_to_column('rowid') |> nest(data=-rowid) |>
  pull(data) |> map(as.numeric) |>
  map_lgl(\(x) pt_in_rect(x, input$V1[1:249], input$V2[1:249]))

max1 <- rects1 |> bind_cols(in_out=in_out1) |>
  filter(!in_out) |>
  mutate(area = (abs(V1.y - V1.x)+ 1)*(abs(V2.y - V2.x) + 1)) |>
  slice_max(area)

rects2 <- input[250:496,] |> cross_join(input[250:496,])
in_out2 <- rects2 |> tibble::rowid_to_column('rowid') |> nest(data=-rowid) |>
  pull(data) |> map(as.numeric) |>
  map_lgl(\(x) pt_in_rect(x, input$V1[250:496], input$V2[250:496]))

max2 <- rects2 |> bind_cols(in_out=in_out2) |>
  filter(!in_out) |>
  mutate(area = (abs(V1.y - V1.x)+ 1)*(abs(V2.y - V2.x) + 1)) |>
  slice_max(area)

max1
max2

ggplot(input) +
  geom_rect(data=max1, mapping=aes(xmin=V1.x, ymin=V2.x, xmax=V1.y, ymax=V2.y), fill='red') +
  geom_path(aes(x=V1, y=V2))

