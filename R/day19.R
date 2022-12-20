#' Day 19: Not Enough Minerals
#'
#' [Not Enough Minerals](https://adventofcode.com/2022/day/19)
#'
#' @name day19
#' @rdname day19
#' @details
#'
#' **Part One**
#'
#' Your scans show that the lava did indeed form obsidian!
#'
#' The wind has changed direction enough to stop sending lava droplets
#' toward you, so you and the elephants exit the cave. As you do, you
#' notice a collection of
#' [geodes](https://en.wikipedia.org/wiki/Geode){target="_blank"} around
#' the pond. Perhaps you could use the obsidian to create some
#' *geode-cracking robots* and break them open?
#'
#' To collect the obsidian from the bottom of the pond, you\'ll need
#' waterproof *obsidian-collecting robots*. Fortunately, there is an
#' abundant amount of clay nearby that you can use to make them waterproof.
#'
#' In order to harvest the clay, you\'ll need special-purpose
#' *clay-collecting robots*. To make any type of robot, you\'ll need *ore*,
#' which is also plentiful but in the opposite direction from the clay.
#'
#' Collecting ore requires *ore-collecting robots* with big drills.
#' Fortunately, *you have exactly one ore-collecting robot* in your pack
#' that you can use to
#' [kickstart]{title="If You Give A Mouse An Ore-Collecting Robot"} the
#' whole operation.
#'
#' Each robot can collect 1 of its resource type per minute. It also takes
#' one minute for the robot factory (also conveniently from your pack) to
#' construct any type of robot, although it consumes the necessary
#' resources available when construction begins.
#'
#' The robot factory has many *blueprints* (your puzzle input) you can
#' choose from, but once you\'ve configured it with a blueprint, you can\'t
#' change it. You\'ll need to work out which blueprint is best.
#'
#' For example:
#'
#'     Blueprint 1:
#'       Each ore robot costs 4 ore.
#'       Each clay robot costs 2 ore.
#'       Each obsidian robot costs 3 ore and 14 clay.
#'       Each geode robot costs 2 ore and 7 obsidian.
#'
#'     Blueprint 2:
#'       Each ore robot costs 2 ore.
#'       Each clay robot costs 3 ore.
#'       Each obsidian robot costs 3 ore and 8 clay.
#'       Each geode robot costs 3 ore and 12 obsidian.
#'
#' (Blueprints have been line-wrapped here for legibility. The robot
#' factory\'s actual assortment of blueprints are provided one blueprint
#' per line.)
#'
#' The elephants are starting to look hungry, so you shouldn\'t take too
#' long; you need to figure out which blueprint would maximize the number
#' of opened geodes after *24 minutes* by figuring out which robots to
#' build and when to build them.
#'
#' Using blueprint 1 in the example above, the largest number of geodes you
#' could open in 24 minutes is *`9`*. One way to achieve that is:
#'
#'     == Minute 1 ==
#'     1 ore-collecting robot collects 1 ore; you now have 1 ore.
#'
#'     == Minute 2 ==
#'     1 ore-collecting robot collects 1 ore; you now have 2 ore.
#'
#'     == Minute 3 ==
#'     Spend 2 ore to start building a clay-collecting robot.
#'     1 ore-collecting robot collects 1 ore; you now have 1 ore.
#'     The new clay-collecting robot is ready; you now have 1 of them.
#'
#'     == Minute 4 ==
#'     1 ore-collecting robot collects 1 ore; you now have 2 ore.
#'     1 clay-collecting robot collects 1 clay; you now have 1 clay.
#'
#'     == Minute 5 ==
#'     Spend 2 ore to start building a clay-collecting robot.
#'     1 ore-collecting robot collects 1 ore; you now have 1 ore.
#'     1 clay-collecting robot collects 1 clay; you now have 2 clay.
#'     The new clay-collecting robot is ready; you now have 2 of them.
#'
#'     == Minute 6 ==
#'     1 ore-collecting robot collects 1 ore; you now have 2 ore.
#'     2 clay-collecting robots collect 2 clay; you now have 4 clay.
#'
#'     == Minute 7 ==
#'     Spend 2 ore to start building a clay-collecting robot.
#'     1 ore-collecting robot collects 1 ore; you now have 1 ore.
#'     2 clay-collecting robots collect 2 clay; you now have 6 clay.
#'     The new clay-collecting robot is ready; you now have 3 of them.
#'
#'     == Minute 8 ==
#'     1 ore-collecting robot collects 1 ore; you now have 2 ore.
#'     3 clay-collecting robots collect 3 clay; you now have 9 clay.
#'
#'     == Minute 9 ==
#'     1 ore-collecting robot collects 1 ore; you now have 3 ore.
#'     3 clay-collecting robots collect 3 clay; you now have 12 clay.
#'
#'     == Minute 10 ==
#'     1 ore-collecting robot collects 1 ore; you now have 4 ore.
#'     3 clay-collecting robots collect 3 clay; you now have 15 clay.
#'
#'     == Minute 11 ==
#'     Spend 3 ore and 14 clay to start building an obsidian-collecting robot.
#'     1 ore-collecting robot collects 1 ore; you now have 2 ore.
#'     3 clay-collecting robots collect 3 clay; you now have 4 clay.
#'     The new obsidian-collecting robot is ready; you now have 1 of them.
#'
#'     == Minute 12 ==
#'     Spend 2 ore to start building a clay-collecting robot.
#'     1 ore-collecting robot collects 1 ore; you now have 1 ore.
#'     3 clay-collecting robots collect 3 clay; you now have 7 clay.
#'     1 obsidian-collecting robot collects 1 obsidian; you now have 1 obsidian.
#'     The new clay-collecting robot is ready; you now have 4 of them.
#'
#'     == Minute 13 ==
#'     1 ore-collecting robot collects 1 ore; you now have 2 ore.
#'     4 clay-collecting robots collect 4 clay; you now have 11 clay.
#'     1 obsidian-collecting robot collects 1 obsidian; you now have 2 obsidian.
#'
#'     == Minute 14 ==
#'     1 ore-collecting robot collects 1 ore; you now have 3 ore.
#'     4 clay-collecting robots collect 4 clay; you now have 15 clay.
#'     1 obsidian-collecting robot collects 1 obsidian; you now have 3 obsidian.
#'
#'     == Minute 15 ==
#'     Spend 3 ore and 14 clay to start building an obsidian-collecting robot.
#'     1 ore-collecting robot collects 1 ore; you now have 1 ore.
#'     4 clay-collecting robots collect 4 clay; you now have 5 clay.
#'     1 obsidian-collecting robot collects 1 obsidian; you now have 4 obsidian.
#'     The new obsidian-collecting robot is ready; you now have 2 of them.
#'
#'     == Minute 16 ==
#'     1 ore-collecting robot collects 1 ore; you now have 2 ore.
#'     4 clay-collecting robots collect 4 clay; you now have 9 clay.
#'     2 obsidian-collecting robots collect 2 obsidian; you now have 6 obsidian.
#'
#'     == Minute 17 ==
#'     1 ore-collecting robot collects 1 ore; you now have 3 ore.
#'     4 clay-collecting robots collect 4 clay; you now have 13 clay.
#'     2 obsidian-collecting robots collect 2 obsidian; you now have 8 obsidian.
#'
#'     == Minute 18 ==
#'     Spend 2 ore and 7 obsidian to start building a geode-cracking robot.
#'     1 ore-collecting robot collects 1 ore; you now have 2 ore.
#'     4 clay-collecting robots collect 4 clay; you now have 17 clay.
#'     2 obsidian-collecting robots collect 2 obsidian; you now have 3 obsidian.
#'     The new geode-cracking robot is ready; you now have 1 of them.
#'
#'     == Minute 19 ==
#'     1 ore-collecting robot collects 1 ore; you now have 3 ore.
#'     4 clay-collecting robots collect 4 clay; you now have 21 clay.
#'     2 obsidian-collecting robots collect 2 obsidian; you now have 5 obsidian.
#'     1 geode-cracking robot cracks 1 geode; you now have 1 open geode.
#'
#'     == Minute 20 ==
#'     1 ore-collecting robot collects 1 ore; you now have 4 ore.
#'     4 clay-collecting robots collect 4 clay; you now have 25 clay.
#'     2 obsidian-collecting robots collect 2 obsidian; you now have 7 obsidian.
#'     1 geode-cracking robot cracks 1 geode; you now have 2 open geodes.
#'
#'     == Minute 21 ==
#'     Spend 2 ore and 7 obsidian to start building a geode-cracking robot.
#'     1 ore-collecting robot collects 1 ore; you now have 3 ore.
#'     4 clay-collecting robots collect 4 clay; you now have 29 clay.
#'     2 obsidian-collecting robots collect 2 obsidian; you now have 2 obsidian.
#'     1 geode-cracking robot cracks 1 geode; you now have 3 open geodes.
#'     The new geode-cracking robot is ready; you now have 2 of them.
#'
#'     == Minute 22 ==
#'     1 ore-collecting robot collects 1 ore; you now have 4 ore.
#'     4 clay-collecting robots collect 4 clay; you now have 33 clay.
#'     2 obsidian-collecting robots collect 2 obsidian; you now have 4 obsidian.
#'     2 geode-cracking robots crack 2 geodes; you now have 5 open geodes.
#'
#'     == Minute 23 ==
#'     1 ore-collecting robot collects 1 ore; you now have 5 ore.
#'     4 clay-collecting robots collect 4 clay; you now have 37 clay.
#'     2 obsidian-collecting robots collect 2 obsidian; you now have 6 obsidian.
#'     2 geode-cracking robots crack 2 geodes; you now have 7 open geodes.
#'
#'     == Minute 24 ==
#'     1 ore-collecting robot collects 1 ore; you now have 6 ore.
#'     4 clay-collecting robots collect 4 clay; you now have 41 clay.
#'     2 obsidian-collecting robots collect 2 obsidian; you now have 8 obsidian.
#'     2 geode-cracking robots crack 2 geodes; you now have 9 open geodes.
#'
#' However, by using blueprint 2 in the example above, you could do even
#' better: the largest number of geodes you could open in 24 minutes is
#' *`12`*.
#'
#' Determine the *quality level* of each blueprint by *multiplying that
#' blueprint\'s ID number* with the largest number of geodes that can be
#' opened in 24 minutes using that blueprint. In this example, the first
#' blueprint has ID 1 and can open 9 geodes, so its quality level is *`9`*.
#' The second blueprint has ID 2 and can open 12 geodes, so its quality
#' level is *`24`*. Finally, if you *add up the quality levels* of all of
#' the blueprints in the list, you get *`33`*.
#'
#' Determine the quality level of each blueprint using the largest number
#' of geodes it could produce in 24 minutes. *What do you get if you add up
#' the quality level of all of the blueprints in your list?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f19a(x)` returns .... For Part Two,
#'   `f19b(x)` returns ....
#' @export
#' @examples
#' f19a(example_data_19())
#' f19b()
f19a <- function(x) {
  # rows are robots, cols are materials
  costs <- lapply(x, parseInput)

  # cols are robots, materials; rows are minutes - 1
  init_materials <- matrix(0, nrow = 25, ncol = length(products))
  colnames(init_materials) <- products
  # init_robots <- matrix(0, ncol = length(robots))
  # init_robots[1, 1] <- 1
  init_factory <- matrix(0, nrow = 25, ncol = length(robots))
  colnames(init_factory) <- robots
  init_factory[c(1, 2), "ore"] <- 1
  # colnames(init_robots) <- robots
  rownames(init_materials) <- rownames(init_factory) <- paste0("t", 0:24)

  # robots <- colSums(factory) ## only update factory at end of each minute
  # BFS finding which robot to make next
  # OR select one of the possible options at random, lots of times

  options(expressions = 5e5) # now we're getting serious
  most_geodes <<- 0
  factory <- init_factory[1,]
  materials <- init_materials[1,]
  blueprint <- costs[[2]]

  geodecache <<- new.env()
  maxg <- eval_blueprint_queue(blueprint, materials, factory, 24)

  # cached_factory <- NULL
  # cached_materials <- NULL
  # set.seed(42)
  #
  # for (it in 1:1e4) {
  #
  #   factory <- init_factory
  #   materials <- init_materials
  #   blueprint <- costs[[1]]
  #   t <- 2
  #
  #   # while t <= 26
  #   while (t <= 25) {
  #     factory[t, ] <- factory[t - 1, ]
  #     materials[t, ] <- materials[t - 1, ]
  #     # prune if there's not enough time to collect enough geodes
  #     # using triangular numbers
  #     if ((materials[t, "geode"] + (24-t)*factory[t, "geode"] + sum(seq(24-t)-1)) < most_geodes) break
  #
  #     too_many <- have_too_many(blueprint, factory[t, ])
  #     too_many["geode"] <- FALSE
  #     build <- can_make(blueprint, materials[t, ]) & !too_many
  #     could_build_next <- can_make(blueprint, materials[t, ] + factory[t, ]) & !too_many
  #     will_build <- c(0, 0, 0, 0)
  #     if (sum(build) == 0) {
  #       TRUE # wait
  #     } else if (build["geode"]) {
  #       build <- 4
  #       will_build[build] <- 1
  #       materials[t, ] <- materials[t, ] - blueprint[build, ]
  #     } else if (build["obsidian"]) {
  #       build <- 3
  #       will_build[build] <- 1
  #       materials[t, ] <- materials[t, ] - blueprint[build, ]
  #     } else if (any(!build & could_build_next)) {
  #       TRUE # don't do anything
  #     } else if (sum(build) == 1 && runif(1) < 0.9) {
  #     # } else if (sum(build) == 1) {
  #       will_build <- as.integer(build)
  #       materials[t, ] <- materials[t, ] - blueprint[build, ]
  #     } else if (sum(build) > 1 && runif(1) < 0.7) {
  #       build <- sample(which(build), 1)
  #       will_build[build] <- 1
  #       materials[t, ] <- materials[t, ] - blueprint[build, ]
  #     }
  #
  #     materials[t, ] <- materials[t, ] + factory[t, ]
  #     factory[t, ] <- factory[t - 1, ] + will_build
  #     t <- t + 1
  #   }
  #
  #   if (materials["t24", "geode"] > most_geodes) {
  #     most_geodes <- materials["t24", "geode"]
  #     cached_materials <- materials
  #     cached_factory <- factory
  #   }
  #
  #   it <- it + 1
  # }
  # most_geodes
  maxg
}

can_make <- function(blueprint, mats) {
  # switch to failthrough for loop for performance?
  apply(blueprint, 1, \(y) all(mats >= y))
}

have_too_many <- function(blueprint, fac) {
  max_cost <- apply(blueprint, 2, max)
  fac > max_cost
}

eval_blueprint_queue <- function(blueprint, materials, factory, t) {
  if (t <= 0) {
    # if (materials["geode"] == 8) browser()
    return(materials["geode"])
  }

  # message("factory:")
  # print(factory)
  # readline()


  # don't keep more materials than we can use to build robots in the remaining time; increases cache collisions
  # max_costs <- apply(blueprint, 2, max)
  # max_costs["geode"] <- 1e3
  # # materials <- setNames(pmin(materials, max_costs), robots)
  # materials <- setNames(pmin(materials, max_costs), robots)

  # discard material if there's not enough time to build a robot per minute with it
  for (m in products) {
    materials[m] <- min(materials[m], max(blueprint[m, ])*t)
  }

  hash <- paste0("id", paste0(c(materials, factory, t), collapse = ""), collapse = "")
  if (exists(hash, envir = geodecache)) {
    # message("alreadh seen this at t = ", t)
    return(get(hash, envir = geodecache))
  }

  # prune if there's not enough time to collect enough geodes
  # using triangular numbers
  if ((materials["geode"] + t*factory["geode"] + sum(seq(t)-1)) < most_geodes) {
    assign(hash, 0, envir = geodecache)
    return(0)
  }


  too_many <- have_too_many(blueprint, factory)
  too_many["geode"] <- FALSE


  build <- can_make(blueprint, materials) & !too_many
  # message("build:")
  # print(build)
  # message("too many:")
  # print(too_many)
  #  could_build_next <- can_make(blueprint, materials + factory) & !too_many
  will_build <- c(0, 0, 0, 0)

  all_next_steps <- c()

  if ( t == 1 || (sum(build) == 0)) {
    # message("nothing to build")
    ## next_able <- 0

    ## tmpm <- materials
    ## while (!build["geode"] && sum(build) == 0 && t > 2 ) { ## CHECK THIS!
    ##     t <- t - 1
    ##     tmpm <- tmpm + factory
    ##     build <- can_make(blueprint, tmpm) & !too_many
    ## }

    # don't bother building anything if this is the final minute
    ## if (t == 1) {
    ##     return(tmpm["geode"])

    next_steps <- eval_blueprint_queue(blueprint, materials + factory, factory, t - 1)
    all_next_steps <- c(all_next_steps, next_steps)
    # hash <- paste0("id", paste0(c(materials + factory, factory, t), collapse = ""), collapse = "")
    # assign(hash, next_steps, envir = geodecache)
  # } else if (build["geode"]) {
  #   # message("can build geode")
  #   build <- 4
  #   will_build[build] <- 1
  #   next_steps <- eval_blueprint_queue(blueprint, materials - blueprint[build, ] + factory, factory + will_build, t - 1)
  #   all_next_steps <- c(all_next_steps, next_steps)
  #   # hash <- paste0("id", paste0(c(materials - blueprint[build, ], factory + will_build, t), collapse = ""), collapse = "")
  #   # assign(hash, next_steps, envir = geodecache)
  } else {
    # message("can build ", toString(robots[which(build)]))
    # explore building better bots first
    for (j in rev(which(build))) {
      will_build <- c(0, 0, 0, 0)
      will_build[j] <- 1
      next_steps <- eval_blueprint_queue(blueprint, materials - blueprint[j, ] + factory, factory + will_build, t - 1)
      all_next_steps <- c(all_next_steps, next_steps)
      # hash <- paste0("id", paste0(c(materials - blueprint[j, ], factory + will_build, t), collapse = ""), collapse = "")
      # assign(hash, next_steps, envir = geodecache)
    }

    # option to do nothing
    next_steps <- eval_blueprint_queue(blueprint, materials + factory, factory, t - 1)
    all_next_steps <- c(all_next_steps, next_steps)
    # hash <- paste0("id", paste0(c(materials, factory, t), collapse = ""), collapse = "")
    # assign(hash, next_steps, envir = geodecache)
  }
  ## will_build[build] <- 1
  ## next_steps <- eval_blueprint_queue(blueprint, tmpm, factory + will_build, t - next_able)
  ## all_next_steps <- c(all_next_steps, next_steps)
  ## hash <- paste0("id", paste0(c(tmpm, factory + will_build, t - next_able), collapse = ""), collapse = "")
  ## assign(hash, next_steps, envir = geodecache)
  # } else if (build["geode"]) {
  #   build <- 4
  #   will_build[build] <- 1
  #   next_steps <- eval_blueprint_queue(blueprint, materials - blueprint[build, ] + factory, factory + will_build, t - 1)
  #   all_next_steps <- c(all_next_steps, next_steps)
  #   hash <- paste0("id", paste0(c(materials - blueprint[build, ] + factory, factory + will_build, t - 1), collapse = ""), collapse = "")
  #   assign(hash, next_steps, envir = geodecache)
  #   ## } else if (build["obsidian"]) {
  #   ##   build <- 3
  #   ##   will_build[build] <- 1
  #   ##   next_steps <- eval_blueprint_queue(blueprint, materials - blueprint[build, ] + factory, factory + will_build, t - 1)
  #   ##   all_next_steps <- c(all_next_steps, next_steps)
  #   ##   hash <- paste0("id", paste0(c(materials - blueprint[build, ] + factory, factory + will_build, t - 1), collapse = ""), collapse = "")
  #   ##   assign(hash, next_steps, envir = geodecache)
  #   ## } else if (any(!build & could_build_next)) {
  #   ##   build <- which(!build & could_build_next)
  #   ##   for (j in build) {
  #   ##     will_build <- c(0, 0, 0, 0)
  #   ##     will_build[j] <- 1
  #   ##     next_steps <- eval_blueprint_queue(blueprint, materials - blueprint[j, ] + 2*factory, factory + will_build, t - 2)
  #   ##     all_next_steps <- c(all_next_steps, next_steps)
  #   ##     hash <- paste0("id", paste0(c(materials - blueprint[j, ] + factory, factory + will_build, t - 1), collapse = ""), collapse = "")
  #   ##     assign(hash, next_steps, envir = geodecache)
  #   ##   }
  #   ##   next_steps <- eval_blueprint_queue(blueprint, materials + factory, factory, t - 1)
  #   ##   all_next_steps <- c(all_next_steps, next_steps)
  # } else {
  #   build <- which(build)
  #   for (j in build) {
  #     will_build <- c(0, 0, 0, 0)
  #     will_build[j] <- 1
  #     next_steps <- eval_blueprint_queue(blueprint, materials - blueprint[j, ] + factory, factory + will_build, t - 1)
  #     all_next_steps <- c(all_next_steps, next_steps)
  #     hash <- paste0("id", paste0(c(materials - blueprint[j, ] + factory, factory + will_build, t - 1), collapse = ""), collapse = "")
  #     assign(hash, next_steps, envir = geodecache)
  #   }
  #   # option to do nothing
  #   next_steps <- eval_blueprint_queue(blueprint, materials + factory, factory, t - 1)
  #   all_next_steps <- c(all_next_steps, next_steps)
  #   hash <- paste0("id", paste0(c(materials + factory, factory, t - 1), collapse = ""), collapse = "")
  #   assign(hash, next_steps, envir = geodecache)
  # }

  # hash <- paste0("id", paste0(c(materials, factory, t), collapse = ""), collapse = "")

  # message("t = ", t)
  # print(all_next_steps)
  max_i <- which.max(all_next_steps)
  max_geodes <- all_next_steps[max_i]
  most_geodes <<- max(most_geodes, max_geodes)
  assign(hash, max_geodes, envir = geodecache)
  return(max_geodes)

}


#' @rdname day19
#' @export
f19b <- function(x) {

}


f19_helper <- function(x) {

}

products <- robots <- c("ore", "clay", "obsidian", "geode")

parseInput <- function(x) {
  bp <- as.integer(sub("Blueprint ([0-9]*):.*", "\\1", x))
  robots <- matrix(0, nrow = length(products), ncol = length(products))
  colnames(robots) <- rownames(robots) <- products
  robots[1,1] <- as.integer(sub(".*ore robot costs ([0-9]*).*", "\\1", x))
  robots[2,1] <- as.integer(sub(".*clay robot costs ([0-9]*).*", "\\1", x))
  robots[3,1] <- as.integer(sub(".*obsidian robot costs ([0-9]*).*", "\\1", x))
  robots[3,2] <- as.integer(sub(".*obsidian robot costs [0-9]* ore and ([0-9]*) clay.*", "\\1", x))
  robots[4,1] <- as.integer(sub(".*geode robot costs ([0-9]*) ore.*", "\\1", x))
  robots[4,3] <- as.integer(sub(".*geode robot costs [0-9]* ore and ([0-9]*) obsidian.*", "\\1", x))
  attr(robots, "blueprint") <- bp
  robots
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day19
#' @export
example_data_19 <- function(example = 1) {
  l <- list(
    a = readLines("inst/example19.txt")
  )
  l[[example]]
}

