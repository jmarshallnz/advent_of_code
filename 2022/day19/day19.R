library(tidyverse)

lines <- 'Blueprint 1: Each ore robot costs 4 ore. Each clay robot costs 2 ore. Each obsidian robot costs 3 ore and 14 clay. Each geode robot costs 2 ore and 7 obsidian.
Blueprint 2: Each ore robot costs 2 ore. Each clay robot costs 3 ore. Each obsidian robot costs 3 ore and 8 clay. Each geode robot costs 3 ore and 12 obsidian.' |>
  str_split('\n') |> unlist()

# ok, extract this shizz
input <- tibble(line = lines) |>
  extract(line, into=c('blueprint', 'ore_ore', 'clay_ore', 'obsidian_ore', 'obsidian_clay', 'geode_ore', 'geode_obsidian'),
          regex=c("Blueprint ([0-9]+): Each ore robot costs ([0-9]+) ore\\. Each clay robot costs ([0-9]+) ore\\. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay\\. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian."),
          convert=TRUE)

# current state
robots <- c(ore = 1, clay = 0, obsidian = 0, geode = 0)
resources <- c(ore = 0, clay = 0, obsidian = 0, geode = 0)

# fixed requirements for a blueprint
robot_requirements <- list(ore = c(4, 0, 0, 0),
                           clay = c(2, 0, 0, 0),
                           obsidian = c(3, 14, 0, 0),
                           geode = c(2, 0, 7, 0))
time_remaining <- 24

buildable_robots <- function(robot_requirements, resources) {
  can_build <- function(requirement, resources) {
    all(resources >= requirement)
  }
  buildable <- map_lgl(robot_requirements, \(x) can_build(x, resources))
  which(buildable)
}

max_geodes <- function(time_remaining, robots, resources) {
  if (time_remaining <= 0)
    return(resources[4]) # no more geodes can be harvested
#  if (robots[4] > 0) {
 #   cat("got a geode bot!\n")
#    if (time_remaining < 3)
#      cat("got robots:", robots, "resources:", resources, '\n')
#  }
  max_geodes <- 0
  # mine resources
  mined_resources = robots
  # branch to build a robot of various types. We can only build one, but our options are any of the robot_requirements
  possible_robot_builds <- buildable_robots(robot_requirements, resources)
  # if we can build a geode, we should, right? Or might building a lower down one be better at some point?
  # it will definitely get more geodes short-term, but would waiting and building one later be better? e.g.
  # maybe better at time T to build another obsidian and at T+1 build the geode so that at T+x we can
  # build another geode? Let's see. That would give us output of another obsidian robot for x-1 time points.
  # which might be enough to sneak another geode bot in at say T=22 which would give 2 geodes instead of 1.
  # let's see.
  for (robot in rev(possible_robot_builds)) {
    # build the robot
    if (robot == 1 && robots[1] >= 4)
      next # no point building another ore bot
    next_robots = robots
    next_robots[robot] <- next_robots[robot] + 1
    next_resources = resources + mined_resources - robot_requirements[[robot]]
    geodes = max_geodes(time_remaining - 1, next_robots, next_resources)
    if (geodes > max_geodes)
      max_geodes = geodes
    if (next_robots[robot] == 1) # our first robot of this type
      break # no point building a lower one instead of this one when we don't have one yet I think?
  }
  # finally try without building a robot as well
  geodes = max_geodes(time_remaining - 1, robots, resources + mined_resources)
  if (geodes > max_geodes)
    max_geodes = geodes
  return(max_geodes)
}

max_geodes(19, robots, resources) # hmm, this is not efficient! :)

# ok, try memoising it?

library(memoise)
max_geodes = memoise(max_geodes)
max_geodes(21, robots, resources) # hmm, this is not efficient at all.
# we possibly need some branch+bound style thing?

system.time({
  g = max_geodes(23, robots, resources) # hmm, this is not efficient at all.
})
g

# we know we can prune any branch that doesn't have obsidian robots

# can we be greedy? We know if we can build a geode bot we SHOULD build a geo, no question?
# (is there a possibility where building something else makes sense?? If we build geode NOW we
# get more geodes, so we should always do so.)
# what about lower down? If we can't build a geode but CAN build an obsidian, should we?
# if we have no obsidian, absolutely! If we have obsidian already, do we need another one?

# does the max geodes at time T+1 always start with max geodes at time T? Or might
# some other route be better? i.e. is it greedy?

# I guess there's a maximum number of ORE robots, right? We never need more than 4 of them
# as can only build 1 robot per round, and 4 would be enough to be completely unconstrained.
# Similarly with sand? The constraint is higher there though so that maybe there's not a point
# considering it. Obsidian is also unconstrained.

# many rounds we shouldn't build a robot though, right?

# 1 geode robot 'costs' 2 ore and 7 obsidian plus 1 time step.
# 1 obsidian costs 3 ore and 14 clay.
# so 7 obsidian costs 3 ore and 14 clay plus 9 time steps (1 for obsidian bot, 7 for resources, 1 for geode bot) OR
# 6 ore and 28 clay plus 6 time steps (2 for obsidian bots, 3 for resources (first bot will do 4, second 3), 1 for geode bot)
# OR 9 ore and 42 clay plus 6 time steps (3 for obsidian bots, 2 for resources (first bot will do 4, second 3, third 2), 1 for geode bot)

# 2 geode robots costs 4 ore and 14 obsidian plus 2 time steps.
# 14 obsidian costs 3 ore and 14 clay plus 15 time steps.
# OR 6 ore and 28 clay plus 11 time steps (2 for o bots, 7 for resources, 2 for geo)
# OR 9 ore and 42 clay plus 9 time steps (3 for o bots, 4 for resources, 2 for geo)

# question: is it ever worth buying obsidian if you can buy a geode now? Going from 1..2 reduces time by 3-4 for 1
# or 2 geodes. But, delaying a geode by 1 removes 1 geode: your second geode is needed at least by time 22.

# what's the earliest we can buy a geode?
# we need 2 ore and 7 obsidian plus 1 time step.
# we can get 2 ore in 2 timesteps (or faster)
# we can get 7 obsidian in 9 time steps plus 3 ore and 14 clay.
# we can get 3 ore and 14 clay in 15 time steps with a clay robot. A clay robot costs 2 ore so 2 time steps.
# so with just 1 clay robot bought as early as possible we'd need 2 time steps for the ore, 1 for the clay robot
# and 14 time steps to get our 3 ore, 14 clay (day 17). After that we'd need another 9 time steps so not doable.
# so we need at least 2 clay robots.

# with 2 clay robots we'd have them at time 5 at which point we'd have 2 clay. We'd then need another 6 time steps
# to get to 14. So time 11 we'd have 14 clay. At time 12 we could buy our obsidian robot. We'd also though have 11-4=7
# ore at that point.

# at time 11 we'd have 14 clay, 7 ore, 2 clay robots, 1 ore robot.
# total resources: 14 clay, 15 ore.

# Which means there's no penalty to buying a clay robot in the meantime.
# let's do that as soon as we can.
# with 3 clay robots we'd have then at time 7 at which point we'd have 6 clay (2 clay from time 5 and 2 a day for 2 days)
# this is purely greedy extension (nice!)
# at time 8 we'd have 9 clay, at time 9, 12 clay at time 10, 15 clay. At time 10 we'd have 10 ore (4 surplus)
# We can buy the obsidian for time 11 when we'd have 15 clay plus 11 ore (3x2 on clay robots, 1x3 on obsidian, 2 surplus)
# There is no possibility of going faster than that.

# At time 11 we'd have: 1 clay, 5 ore, 1 obsidian robot, 3 clay robots, 1 ore robot.
# Total resources sunk: 15 clay, 15 ore.

# Clearly better solution?

# what if clay robots cost 3?

# after time 7 we'd have 3 clay, 1 ore, 1 or, 2 cr.
# after time 10 we'd have 11 clay, 4 ore, 1 or, 2 cr, OR 11 clay, 1 ore, 1 or, 3 cr
# after time 11 we'd have 13 clay, 5 ore, 1 or, 2 cr, OR 14 clay, 2 ore, 1 or, 3 cr
# after time 12 we'd have 15 clay, 6 ore, 1 or, 2 cr, OR 17 clay, 3 ore, 1 or, 3 cr || either case we get obsidian

# what if clay robots cost 4?

# after time 9 we'd have 4 clay, 1 ore, 1 or, 2 cr.
# after time 13 we'd have 12 clay, 5 ore, 1 or, 2 cr, OR 12 clay, 1 ore, 1 or, 3 cr.
# after time 14 we'd have 14 clay, 6 ore, 1 or, 2 cr, OR 15 clay, 2 ore, 1 or, 3 cr.
# after time 15 we'd have 2 clay, 4 ore, 1 or, 2 cr, 1 ob, OR 16 clay, 8 ore, 1 or, 2 cr, OR 18 clay, 3 ore, 1 or, 3 cr.

# which one is better now? (16 clay, 15 ore), (16 clay, 15 ore) + better robot, (18 clay, 15 ore) + 1 better robot.

# maybe potential earnings?

# 1. we have 16 clay, 15 ore, but have future earnings of 1 ore, 2 clay, 1 obs/day plus future purchases...
# 2. we have 16 clay, 15 ore, but have future earnings of 1 ore, 2 clay/day. And excess ore/clay (seems bad)
# 3. we have 18 clay, 15 ore, but have future earnings of 1 ore, 3 clay/day plus future purchases...

# It is clear that 1 is better than 3? Presumably 1 is better than 2 due to the future earnings.
# we need to evaluate them in terms of future possible geodes (basically minimum geodes we can "buy")


# OK, so upper bound can work with "best case" e.g. if we have T minutes left, then at best we can build...
# T-1 geodes robots? 1 minute left -> no robots. 2 minutes left -> max 1 geode more. 3 minutes left, max 3 geodes
# more.

# we can also iterate on resource costs instead of builds?
# i.e. time step further?


# after time 1 we'd have 11 clay, 4 ore, 1 or, 2 cr, OR 11 clay, 1 ore, 1 or, 3 cr
# after time 11 we'd have 13 clay, 5 ore, 1 or, 2 cr, OR 14 clay, 2 ore, 1 or, 3 cr
# after time 12 we'd have 15 clay, 6 ore, 1 or, 2 cr, OR 17 clay, 3 ore, 1 or, 3 cr || either case we get obsidian

# if we instead buy an ore robot, the best we can get is (costs 4 ore) the best we can do is double our ore
# collecting speed at the cost of 4 timesteps.

# so timesteps_obs = max(14/clay_robots + 3/ore_robots). We always want at least 4 clay robots per ore robot.
# so given 1 ore robot, we must max out the clay robots first.

# get a clay robot if 14/3*ore_robots > clay_robots; Else consider either?

# ok, then timesteps_geo = max(7/obs_robots + 2/ore_robots). So we want at least 3 obs_robots per ore robot.
# So prioritise obsidian robots at least until we have.

# whether we buy geo or obs if given the chance possibly doesn't matter as much?


# That's not the best we can do though. While we're waiting for that clay we could build another
# do. We have a number of options at time 11: robots 1,2,1; resources 1,

# Better option: iterate on which robot to build next. This gets rid of all the "don't build one" stuff.
# and we should always be building robots I think (at least up until the end?)

# The logic here is to iterate on the decision of which is the next robot to build
# (as presumably there'll be fewer robots built than timesteps, so less depth?)
# Then prune aggressively.
# The BIGGEST timesave is to branch and bound: The really simple bound is just
# what if we can build geode robots from here on out? This gives a quadratic number of
# geodes (triangle sequence)
# That reduced runtime for P2 from 10+ hours to 2 seconds...
max_geodes <- function(time_remaining, robot_requirements, max_geodes, robots, resources) {
  if (time_remaining <= 0) {
 #   cat('best so far:', robots, resources, "\n")
    return(resources[4]) # no more geodes can be harvested
  }
  #  if (robots[4] > 0) {
  #   cat("got a geode bot!\n")
  #    if (time_remaining < 3)
  #      cat("got robots:", robots, "resources:", resources, '\n')
  #  }
#  cat("time", time_remaining, "robots", robots, "resources", resources, "\n")

  max_additional_geode = time_remaining * robots[4] + time_remaining * (time_remaining - 1) / 2
  if (resources[4] + max_additional_geode <= max_geodes) {
    return(max_geodes)
  }

  # we could potentially build one of 4 bots next.
  bots_to_build <- 4:1
  # first off, no point building one we don't need, so rule them out.
  if (robots[2] == 0) {
    # can't build an obsidian bot if we have no clay
    bots_to_build = 2:1
  } else if (robots[3] == 0) {
    # can't build a geode if we have no obsidian
    bots_to_build = 3:1
  }
  # sometimes there's no point building anything!
  if (time_remaining == 1) {
    bots_to_build = c()
  } else if (time_remaining == 2) {
    bots_to_build = setdiff(bots_to_build, 1:3)
  }
  # and sometimes we have more than we need already?
  if (robots[1] >= 4) {
    bots_to_build = setdiff(bots_to_build, 1)
  }
  # now check when we can build each bot and branch accordingly
  built_robot = FALSE
  for (i in bots_to_build) {
    # find how long it'll take to build this (we may need to collect more resources)
    time_to_build = max(c(0, ceiling((robot_requirements[i,] - resources)/robots)), na.rm=TRUE) + 1
 #   if (i == 4) {
#      cat("building geode!!", time_to_build, "remaining=", time_remaining, "res=", resources, "rob=", robots, "\n")
#    }
    if (time_remaining >= time_to_build) { # no point building if it'll take longer than we need
      # and branch to that time point
 #     if (i == 4) {
#        cat("building obsidian!", time_remaining, resources, robots, "\n")
#      }
      next_time_remaining = time_remaining - time_to_build
      next_robots = robots; next_robots[i] = next_robots[i] + 1
      next_resources = resources + time_to_build*robots - robot_requirements[i,]
 #     cat("building", i, "takes", time_to_build, "\n")
#      cat("remaining", time_remaining, "->", next_time_remaining, "\n")
#      cat("resources = ", resources, "->", next_resources, "\n")
#      cat("robots = ", robots, "->", next_robots, "\n")
      #      cat("robots = ", robots, "\n")
      geodes = max_geodes(next_time_remaining, robot_requirements, max_geodes, next_robots, next_resources)
      if (geodes > max_geodes)
        max_geodes = geodes
      built_robot = TRUE
    }
  }
  if (!built_robot) {
    # we can't build any robots, so go ahead and run out our time
    geodes = max_geodes(0, robot_requirements, max_geodes, robots, resources + time_remaining*robots)
    if (geodes > max_geodes)
      max_geodes = geodes
  }
  return(max_geodes)
}

system.time({
g <- max_geodes(24, robots, resources) # hmm, 9 after 24 in 10 secs isn't too bad I guess?
})
g

# fixed requirements for a blueprint
robot_requirements <- tibble(ore = c(2, 0, 0, 0),
                           clay = c(3, 0, 0, 0),
                           obsidian = c(3, 8, 0, 0),
                           geode = c(3, 0, 12, 0)) |>
  as.matrix() |> t()

system.time({
  g <- max_geodes(24, robots, resources) # hmm, 9 after 24 in 10 secs isn't too bad I guess?
})
g

# ok, run over our blueprints
get_requirements <- function(row) {
  tibble(ore = c(row$ore_ore, 0, 0, 0),
                             clay = c(row$clay_ore, 0, 0, 0),
                             obsidian = c(row$obsidian_ore, row$obsidian_clay, 0, 0),
                             geode = c(row$geode_ore, 0, row$geode_obsidian, 0)) |>
    as.matrix() |> t()
}

reqs <- input |>
  rowwise() |>
  group_split() |>
  map(get_requirements)

mirai::daemons(2)
ans <- map_int(reqs, in_parallel(\(x) max_geodes(24, x, robots, resources), max_geodes=max_geodes,
                                 robots = robots, resources = resources), .progress = TRUE)
mirai::daemons(0)

lines <- readLines('2022/day19/input.txt')

# ok, extract this shizz
input <- tibble(line = lines) |>
  extract(line, into=c('blueprint', 'ore_ore', 'clay_ore', 'obsidian_ore', 'obsidian_clay', 'geode_ore', 'geode_obsidian'),
          regex=c("Blueprint ([0-9]+): Each ore robot costs ([0-9]+) ore\\. Each clay robot costs ([0-9]+) ore\\. Each obsidian robot costs ([0-9]+) ore and ([0-9]+) clay\\. Each geode robot costs ([0-9]+) ore and ([0-9]+) obsidian."),
          convert=TRUE)

reqs <- input |>
  rowwise() |>
  group_split() |>
  map(get_requirements)

mirai::daemons(2)
ans <- map_int(reqs, in_parallel(\(x) max_geodes(24, x, 0, robots, resources), max_geodes=max_geodes,
                                 robots = robots, resources = resources), .progress = TRUE)
mirai::daemons(0)
ans
tibble(geodes = ans) |> rowid_to_column('id') |>
  mutate(quality = id*geodes) |>
  summarise(sum(quality)) #994

# always frustrating when the given input works, eh?
# number 2 says it's 0...
# prob need to buy an ore?
# so time 5 we'd have 1 ore, 2 rob_ores
# time 7 we'd have 5 ore, 2 rob_ores
# time 8 we'd have 3 ore, 2 rob_ores, 1 rob_clay
# time 9 we'd have 5 ore, 1 clay, 2 rob_ores, 1 rob_clay
# time 10 we'd have 3 ore, 2 clay, 2 rob_ores, 2 rob_clay
# time 11 we'd have 5 ore, 4 clay, 2 rob_ores, 2 rob_clay
# time 12 we'd have 3 ore, 6 clay, 2 rob_ores, 3 rob_clay
# time 13 we'd have 5 ore, 9 clay, 2 rob_ores, 3 rob_clay
# time 14 we'd have 3 ore, 12 clay, 2 rob_ores, 4 rob_clay
# time 15 we'd have 5 ore, 4 clay, 2 rob_ores, 4 rob_clay, 1 rob_obs
# time 16 we'd have 3 ore, 8 clay, 1 obs, 2 rob_ores, 5 rob_clay, 1 rob_obs
# time 17 we'd have 5 ore, 13 clay, 2 obs, 2 rob_ores, 5 rob_clay, 1 rob_obs
# time 18 we'd have 3 ore, 6 clay, 3 obs, 2 rob_ores, 5 rob_clay, 2 rob_obs
# time 19 we'd have 5 ore, 11 clay, 5 obs, 2 rob_ores, 5 rob_clay, 2 rob_obs
# time 20 we'd have 7 ore, 16 clay, 7 obs, 2 rob_ores, 5 rob_clay, 2 rob_obs
# time 21 we'd have 9 ore, 21 clay, 9 obs, 2 rob_ores, 5 rob_clay, 2 rob_obs
# time 22 we'd have 8 ore, 26 clay, 3 obs, 2 rob_ores, 5 rob_clay, 2 rob_obs, 1 rob_geo # YAY?
# time 23 we'd have 8 ore, 26 clay, 5 obs, 1 geo, 2 rob_ores, 5 rob_clay, 2 rob_obs, 1 rob_geo
# time 24 we'd have 8 ore, 26 clay, 7 obs, 2 geo, 2 rob_ores, 5 rob_clay, 2 rob_obs, 1 rob_geo

max_geodes(24, reqs[[2]], robots, resources)

# Part 2 levels it up: we're now at 32...

# t=28 is about 50 secs with 2 daemons
mirai::daemons(2)
ans <- map_int(reqs[1:3], in_parallel(\(x) max_geodes(28, x, robots, resources), max_geodes=max_geodes,
                                 robots = robots, resources = resources), .progress = TRUE)
mirai::daemons(0)

# t=28 is about 47 secs with 3 daemons?
mirai::daemons(3)
ans <- map_int(reqs[1:3], in_parallel(\(x) max_geodes(28, x, robots, resources), max_geodes=max_geodes,
                                      robots = robots, resources = resources), .progress = TRUE)
mirai::daemons(0)

# t=29 is about 140 secs with 3 daemons
mirai::daemons(3)
ans <- map_int(reqs[1:3], in_parallel(\(x) max_geodes(29, x, robots, resources), max_geodes=max_geodes,
                                      robots = robots, resources = resources), .progress = TRUE)
mirai::daemons(0)

# t=30 is about 420 secs with 3 daemons (so about 3 times I guess?)
# t=31 should be about 21 mins or so. Turned out to be 33 mins, or ~5 times as long.
# so t=32 will be maybe a few hours

# Nice, adding the max constraint (way better branch and bound) reduces a 10 hour run down to 2 seconds...
mirai::daemons(3)
start_time = Sys.time()
ans <- map_int(reqs[1:3], in_parallel(\(x) max_geodes(32, x, 0, robots, resources), max_geodes=max_geodes,
                                      robots = robots, resources = resources), .progress = TRUE)
end_time = Sys.time()
end_time - start_time
prod(ans)
mirai::daemons(0)
