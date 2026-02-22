library(tidyverse)

input <- read.table(text='2,2,2
1,2,2
3,2,2
2,1,2
2,3,2
2,2,1
2,2,3
2,2,4
2,2,6
1,2,5
3,2,5
2,1,5
2,3,5', sep=',') |>
  set_names(c('x', 'y', 'z')) |>
  tibble::rowid_to_column('cube')

input <- read.table("2022/day18/input.txt", sep=',') |>
  set_names(c('x', 'y', 'z')) |>
  tibble::rowid_to_column('cube')

# ok, each cube has 6 faces except where they touch another cube. Cubes touch
# if two columns are the same and one differs by 1. Can do this with a join.

nbhood <- input |> mutate(x1 = x-1, x2 = x+1,
                y1 = y-1, y2 = y+1,
                z1 = z-1, z2 = z+1) |>
  rename(nb_cube = cube)

z_nb <- input |> left_join(nbhood, by=join_by(x, y, between(z, z1, z2))) |>
  filter(z.x != z.y) |> select(cube, x, y, z=z.x, nb_cube)
y_nb <- input |> left_join(nbhood, by=join_by(x, z, between(y, y1, y2))) |>
  filter(y.x != y.y) |> select(cube, x, y=y.x, z, nb_cube)
x_nb <- input |> left_join(nbhood, by=join_by(y, z, between(x, x1, x2))) |>
  filter(x.x != x.y) |> select(cube, x=x.x, y, z, nb_cube)

cubies <- input |> left_join(bind_rows(x_nb, y_nb, z_nb))
cubies |>
  group_by(cube) |>
  summarise(faces = 6 - sum(!is.na(nb_cube))) |>
  summarise(sum(faces))

# Part 2 we have to also preclude air-pockets...

# the droplet is within the 22x22x22 cube, which is only ~10k or so blocks.
# so we an interior only thing would be one which you can't reach from the
# outside. This *could* be tricky. Let's visualise. It's basically a noisy
# sphere:
ggplot(input) +
  aes(x=x, y=y) +
  geom_tile() +
  facet_wrap(vars(z))

ggplot(input) +
  aes(x=x, y=z) +
  geom_tile() +
  facet_wrap(vars(y))

# plausibly we might do it with a graph: build a 6-connected graph of the cube
# and then check which 'red' cubies are connected to the outside ones?

# or we could 'flood-fill' the outside (as the inside has multiple holes by the
# looks)

outside <- array(0, dim=c(22, 22, 22))
outside[input |> select(x, y, z) |> mutate(across(everything(), \(x) x+1)) |> as.matrix()] <- 1

library(collections)

# naive algorithm gets stack overflow, so use a list
flood_fill <- function(outside, x, y, z) {
  q <- stack(items = list(cbind(x, y, z)))
  while (q$size()) {
    node <- q$pop()
    if (outside[node] == 0) { # point is unfilled (could be inside or outside)
      # set the node
      outside[node] <- 2
      # flood fill in 3d
      for (i in 1:ncol(node)) {
        if (node[i] < dim(outside)[i]) {
          n = node; n[i] = n[i]+1; q$push(n)
        }
        if (node[i] > 1) {
          n = node; n[i] = n[i]-1; q$push(n)
        }
      }
    }
  }
  return(outside)
}

test <- flood_fill(outside, 1, 1, 1)

sum(test == 2) # seems to have worked
sum(test == 1)
sum(test == 0)

# ok, now go to long-form and part1 it with the 0's added in
inside <- which(test == 0, arr.ind=TRUE) |>
  as.data.frame() |> set_names('x', 'y', 'z') |>
  mutate(across(everything(), \(x) x - 1))
inside

# should have no overlap:
inside |> semi_join(input) # yep
input |> semi_join(inside) # yep

# ok, now add them all together then part 1
filled_blob <- bind_rows(inside=inside, cube=input |> select(x,y,z), .id='type') |>
  tibble::rowid_to_column('cube')

nbhood <- filled_blob |> mutate(x1 = x-1, x2 = x+1,
                          y1 = y-1, y2 = y+1,
                          z1 = z-1, z2 = z+1) |>
  rename(nb_cube = cube)

z_nb <- filled_blob |> left_join(nbhood, by=join_by(x, y, between(z, z1, z2))) |>
  filter(z.x != z.y) |> select(cube, x, y, z=z.x, nb_cube)
y_nb <- filled_blob |> left_join(nbhood, by=join_by(x, z, between(y, y1, y2))) |>
  filter(y.x != y.y) |> select(cube, x, y=y.x, z, nb_cube)
x_nb <- filled_blob |> left_join(nbhood, by=join_by(y, z, between(x, x1, x2))) |>
  filter(x.x != x.y) |> select(cube, x=x.x, y, z, nb_cube)

cubies <- filled_blob |> left_join(bind_rows(x_nb, y_nb, z_nb))

cubies |>
  group_by(type, cube) |>
  summarise(faces = 6 - sum(!is.na(nb_cube))) |>
  summarise(sum(faces))
