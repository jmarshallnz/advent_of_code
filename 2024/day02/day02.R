library(tidyverse)

d <- read.table("day2/input.txt", header=FALSE, fill=TRUE)

# pivot longer so we can do stuff grouped (classic tidyverse trick)
d_long <- d |> tibble::rowid_to_column() |>
  pivot_longer(-rowid)

# part 1
d_long |>
  group_by(rowid) |>
  arrange(rowid, name) |>
  mutate(diff = value - lag(value)) |>
  filter(!is.na(diff)) |>
  summarise(all_pos = all(diff > 0),
            all_neg = all(diff < 0),
            all_small = all(abs(diff) <= 3),
            ok = (all_pos | all_neg) & all_small) |>
  filter(ok) -> ok_rows

# check if reasonable
d_long |> semi_join(ok_rows)
d_long |> anti_join(ok_rows)

# number of rows is the answer
ok_rows |> nrow()

# part 2: one bad report is OK
# hmm, could just do the above then iterate on which one is removed. Clunky in idea,
# but could possibly become a bit more elegant if we just duplicate the dataset
# with each column in term removed. We can do this with rsample library to create
# a cross-validation set where the 'assessment' sets contain the column we wish
# to remove. This leaves all the other columns in the analysis sets (which is what we want!)
# rows are OK if at least one of the analysis sets are ok, noting we don't have to
# worry about the case where the row satisfies part 1, as removing the first or last
# column would mean it's still OK anyway. (unless there are entries with only 1 column...)
library(rsample)

d_split <- d_long |> group_vfold_cv(group=name)
d_split |>
  mutate(d = map(splits, analysis)) |>
  select(id, d) |>
  unnest(d) |>
  group_by(id, rowid) |>
  arrange(id, rowid, name) |>
  mutate(diff = value - lag(value)) |>
  filter(!is.na(diff)) |>
  summarise(all_pos = all(diff > 0),
            all_neg = all(diff < 0),
            all_small = all(abs(diff) <= 3),
            ok = (all_pos | all_neg) & all_small) |>
  group_by(rowid) |>
  summarise(ok = sum(ok) > 0) |>
  filter(ok) -> ok_rows2

d_long |> semi_join(ok_rows2) |> print(n=100)
d_long |> anti_join(ok_rows2) |> print(n=100)

ok_rows2 |> nrow()
