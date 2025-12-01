library(tidyverse)



# ok, write our program to produce new secret numbers

#Calculate the result of multiplying the secret number by 64. Then, mix this result into the secret number. Finally, prune the secret number.
#Calculate the result of dividing the secret number by 32. Round the result down to the nearest integer. Then, mix this result into the secret number. Finally, prune the secret number.
#Calculate the result of multiplying the secret number by 2048. Then, mix this result into the secret number. Finally, prune the secret number.

prune_mix <- function(secret, value) {
  bitwXor(secret %% 16777216, value %% 16777216)  # powers of two
}

next_secret <- function(secret) {
  secret <- prune_mix(secret, secret*64)
  secret <- prune_mix(secret, secret%/%32)
  secret <- prune_mix(secret, secret*2048)
}

input <- read.table("day22/input.txt") |> pull(V1)
#input <- c(1,2,3,2024)
#input <- 123

secrets <- matrix(0, length(input), 2001)
secrets[,1] <- input
for (i in 2:2001) {
  secrets[,i] <- next_secret(secrets[,i-1])
}
sum(secrets[,2001])

secrets_10 <- secrets %% 10
bananas <- secrets_10[,-1]
diffs  <- t(apply(secrets_10, 1, diff)) + 10 # 10 to give us a range
# The required sequence only applies once per buyer, and only the first

# now just run across the columns, getting our sequences
totals <- array(0, dim=c(19,19,19,19,nrow(diffs))) # probably should use some sort of list/map instead...
for (i in 4:ncol(diffs)) {
  ind <- cbind(diffs[,(i-3):i], seq_len(nrow(diffs)))
  totals[ind] <- if_else(totals[ind] > 0, totals[ind], bananas[,i])
}

# ok, now find out which item has the most
bananas_per_sequence <- apply(totals, 1:4, sum)
max(bananas_per_sequence)
