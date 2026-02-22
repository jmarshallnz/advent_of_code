library(tidyverse)
library(bit)

to_bits <- function(hex) {
  msb_bits <- function(x) {
    rawToBits(x) |>
      as.integer() |> rev()
  }
  bytes <- paste0(hex[c(TRUE, FALSE)],
                 hex[c(FALSE, TRUE)])
  bits <- strtoi(bytes, base=16) |>
    as.raw() |>
    map(msb_bits) |>
    unlist()
  bits
}
to_int <- function(bits) {
  sum(bits*2^rev(seq_along(bits)-1))
}

# decode packets
read_packet <- function(bits) {

  pos <- 0
  version <- to_int(bits[pos + 1:3]); pos <- pos + 3
  type_id <- to_int(bits[pos + 1:3]); pos <- pos + 3

  if (type_id == 4) {
    # literal
    num <- 0
    while(TRUE) {
      num <- num*16 + to_int(bits[pos + 2:5]);
      if (bits[pos + 1] == 0) # done
        break
      pos <- pos + 5
    }
    return(list(version = version,
               type_id = type_id,
               length  = pos+5,
               number  = num))
  } else {
    # read subpackets
    subpackets <- list()
    len_type <- bits[pos+1]; pos <- pos + 1
    if (len_type == 0) {
      len_in_bits <- to_int(bits[pos+1:15]); pos <- pos + 15
      cur_len <- 0
      while (cur_len < len_in_bits) {
        subpacket = read_packet(bits[pos + seq_len(length(bits)-pos)])
        cur_len <- cur_len + subpacket$length
        pos <- pos + subpacket$length
        subpackets[[length(subpackets)+1]] <- subpacket
      }
      # add our packet
      return(list(version = version,
                  type_id = type_id,
                  length = pos,
                  packets = subpackets))
    } else {
      num_subpackets <- to_int(bits[pos+1:11]); pos <- pos + 11
      for (i in 1:num_subpackets) {
        subpackets[[i]] <- read_packet(bits[pos+seq_len(length(bits)-pos)])
        pos <- pos + subpackets[[i]]$length
      }
      # add our packet
      return(list(version = version,
                  type_id = type_id,
                  length = pos,
                  packets = subpackets))
    }
  }
}

# convert to bits
hex <- "D2FE28" |> str_split('') |> unlist()
bits <- to_bits(hex)
read_packet(bits)

read_hex_string <- function(str) {
  # split up hex string
  hex <- str |> str_split('') |> unlist()
  # convert to bits
  bits <- to_bits(hex)
  # and read packets
  read_packet(bits)
}

read_hex_string(str = "D2FE28")
read_hex_string(str = "38006F45291200")

# OK, now count the versions
add_versions <- function(packet) {
  add <- packet$version
  if (packet$type_id != 4) { # has subpackets
    # has subpackets
    for (p in packet$packets) {
      add <- add + add_versions(p)
    }
  }
  return(add)
}

read_hex_string(str = "38006F45291200") |> add_versions()
read_hex_string(str = "EE00D40C823060") |> add_versions()
read_hex_string(str = "8A004A801A8002F478") |> add_versions()
read_hex_string(str = "620080001611562C8802118E34") |> add_versions()
read_hex_string(str = "C0015000016115A2E0802F182340") |> add_versions()
read_hex_string(str = "A0016C880162017C3686B18A3D4780") |> add_versions()

# OK, all good :)
hex_str <- readLines('2021/day16/input.txt')
read_hex_string(hex_str) |> add_versions()

# part 2: computing from our tree
packet <- read_hex_string(hex_str)

compute <- function(packet) {
  # check our packet type
  if (packet$type_id == 4) { # literal
    return(packet$number)
  }
  # operator: run over all our subpackets, then compute
  values <- map_dbl(packet$packets, compute)
  ans <- switch(as.character(packet$type_id),
     '0' = sum(values),
     '1' = prod(values),
     '2' = min(values),
     '3' = max(values),
     '5' = ifelse(values[1] > values[2], 1, 0),
     '6' = ifelse(values[1] < values[2], 1, 0),
     '7' = ifelse(values[1] == values[2], 1, 0))
  return(ans)
}

compute(packet)

# testing
read_hex_string(str='C200B40A82') |> compute()
read_hex_string(str='04005AC33890') |> compute()
read_hex_string(str='880086C3E88112') |> compute()
read_hex_string(str='CE00C43D881120') |> compute()
read_hex_string(str='D8005AC2A8F0') |> compute()
read_hex_string(str='F600BC2D8F') |> compute()
read_hex_string(str='9C005AC2F8F0') |> compute()
read_hex_string(str='9C0141080250320F1802104A08') |> compute()
