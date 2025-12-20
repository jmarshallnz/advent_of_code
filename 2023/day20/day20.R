library(tidyverse)
options(digits = 22,
        pillar.max_dec_width = 22)

# each module has a state (on/off for flipflops, high/low array for conjunctions)
# each pulse they change states and send pulses

# first step is working backwards from each conjunction to get their
# inputs and setting up states
# then we process the pulses which can be high/low and have a destination

raw_input <- readLines("2023/day20/input.txt")
raw_input <- "broadcaster -> a, b, c
%a -> b
%b -> c
%c -> inv
&inv -> a" |>str_split("\n") |>
  unlist()

raw_input <- "broadcaster -> a
%a -> inv, con
&inv -> b
%b -> con
&con -> output" |>str_split("\n") |>
  unlist()

input <- raw_input |>
  str_split_fixed(" -> ", n=2) |>
  as.data.frame() |>
  extract(V1, into=c("type", "node"), regex="([%&]?)(.*)") |>
  mutate(to = str_split(V2, ", "), .keep='unused') |>
  mutate(inputs = map(node, \(x) node[map_lgl(to, \(t) any(t==x))]))

# ok, now we have our node set we can do our pulses, but first
# we need to knwo the input to our conjunctions so we can deal
# with those
flip_flops <- input |> filter(type == "%") |> select(node, to) |>
  nest(data=to) |>
  deframe() |>
  map(\(x) { x = list(type='flipflop', to = x$to[[1]], state=FALSE) } ) # grrr

conjunctions <- input |> filter(type == "&") |> select(node, to, inputs) |>
  nest(data=c(to, inputs)) |>
  deframe() |>
  map(\(x) { x = list(type='conjunction', to = x$to[[1]], state = rep(FALSE, length(x$inputs[[1]])) |> as.list() |> set_names(x$inputs[[1]])) } ) # grrr

# add any additional unspecified nodes
extras <- tibble(node = input$to |> unlist() |> setdiff(input$node), dump=1) |>
  deframe() |> map(\(x) { x = list(type='')})

# combine them all
modules <- c(flip_flops, conjunctions, extras)

# OK, so fire a button push to the broadcaster


# right, now we run through our pulses, sending them through
# the machines in order. If we get another pulse it's tacked on the end

press_button <- function(modules, monitor_lows = NULL) {
  # grab our first pulses
  pulses <- input |> filter(node == "broadcaster") |>
    unnest('to') |>
    select(from=node, to) |>
    mutate(high = FALSE) |>
    rowwise() |> group_split() |>
    map(as.list)

  monitor_pulsed_with_low <- FALSE
  num_high_low <- c(1, 0)
  while(length(pulses) >= 1) {
    # first pulse (this could be a queue if we need speed)
    pulse <- pulses[[1]]
    pulses <- pulses[-1]
  #  cat("pulse:", pulse$from, "to", pulse$to, "state=", pulse$high, "\n")
    # increment our high/low counter
    num_high_low[1+pulse$high] <- num_high_low[1+pulse$high]+1
    # if the pulse is high and we're monitoring this module
    # then set our flag
    if (!is.null(monitor_lows) && pulse$to == monitor_lows &&
        !pulse$high) {
      monitor_pulsed_with_low <- TRUE
    }
    # send the pulse
    mod <- modules[[pulse$to]]
    if (mod$type == "flipflop") {
      if (pulse$high) {
        # nothing happens
      } else {
        # toggle state and send a pulse
        mod$state <- !mod$state

        # need to add a pulse for each to
        for (to in mod$to) {
          pulses[[length(pulses)+1]] <- list(from=pulse$to, to=to, high=mod$state)
        }
      }
    } else if (mod$type == "conjunction") { # conjunction
      # update memory
      mod$state[[pulse$from]] <- pulse$high
      state <- !all(mod$state == TRUE) # send low pulse if everything is high

      # add a pulse for each to
      for (to in mod$to) {
        pulses[[length(pulses)+1]] <- list(from=pulse$to, to=to, high=state)
      }
    }
    # update our module state
    modules[[pulse$to]] <- mod
  }
  return(lst(modules, num_high_low, monitor_pulsed_with_low))
}

# part 1 solution:
modules <- c(flip_flops, conjunctions, extras)
total_high_low <- c(0, 0)
for (i in 1:1000) {
  cat('press button', i, '\n')
  out <- press_button(modules)
  modules <- out$modules
  total_high_low <- total_high_low + out$num_high_low
}
prod(total_high_low)

# Part 2: we need to keep pressing until rx receives a low pulse.

# Start by taking a look at the network:
input |>
  select(node, to) |>
  unnest(to) |>
  as.matrix() |>
  graph_from_edgelist(directed=FALSE) |>
  set_vertex_attr(name='color', index=input$node, value=ifelse(input$type == "&", 'lightblue', 'pink')) |>
  plot(vertex.size = 10)

# ok, byt he looks rx is fed by a conjunction (ll) which is in turn fed
# by four other conjunctions (inverters) which are in turn fed
# by conjunctions that are independent (gv, qf, rc, ll), fed via a bunch of flipflops
# from broadcaster.

# so we need to know when gv, qf, rc and ll switch state to all
# high, and thus send a low pulse to those inverters. It's likely
# that they do so on a period?

first_pulsed_low <- function(module, max_presses = 10000) {
  modules <- c(flip_flops, conjunctions, extras)
  for (i in 1:max_presses) {
    if (i %% 1000 == 0) cat('press button', i, '\n')
    out <- press_button(modules, monitor = module)
    modules <- out$modules
    if (out$monitor_pulsed_with_low)
      return(i)
  }
  return(0)
}

first_low <- tibble(monitor = c('xt', 'zc', 'fp', 'mk')) |>
  mutate(low = map_int(monitor, first_pulsed_low))

first_low # these are all prime - classic

first_low |>
  summarise(prod(low))
