#' Day 24: Never Tell Me The Odds
#'
#' [Never Tell Me The Odds](https://adventofcode.com/2023/day/24)
#'
#' @name day24
#' @rdname day24
#' @details
#'
#' **Part One**
#'
#' It seems like something is going wrong with the snow-making process.
#' Instead of forming snow, the water that\'s been absorbed into the air
#' seems to be forming
#' [hail](https://en.wikipedia.org/wiki/Hail){target="_blank"}!
#' 
#' Maybe there\'s something you can do to break up the hailstones?
#' 
#' Due to strong, probably-magical winds, the hailstones are all flying
#' through the air in perfectly linear trajectories. You make a note of
#' each hailstone\'s *position* and *velocity* (your puzzle input). For
#' example:
#' 
#'     19, 13, 30 @ -2,  1, -2
#'     18, 19, 22 @ -1, -1, -2
#'     20, 25, 34 @ -2, -2, -4
#'     12, 31, 28 @ -1, -2, -1
#'     20, 19, 15 @  1, -5, -3
#' 
#' Each line of text corresponds to the position and velocity of a single
#' hailstone. The positions indicate where the hailstones are *right now*
#' (at time `0`). The velocities are constant and indicate exactly how far
#' each hailstone will move in *one nanosecond*.
#' 
#' Each line of text uses the format `px py pz @ vx vy vz`. For instance,
#' the hailstone specified by `20, 19, 15 @ 1, -5, -3` has initial X
#' position `20`, Y position `19`, Z position `15`, X velocity `1`, Y
#' velocity `-5`, and Z velocity `-3`. After one nanosecond, the hailstone
#' would be at `21, 14, 12`.
#' 
#' Perhaps you won\'t have to do anything. How likely are the hailstones to
#' collide with each other and smash into tiny ice crystals?
#' 
#' To estimate this, consider only the X and Y axes; *ignore the Z axis*.
#' Looking *forward in time*, how many of the hailstones\' *paths* will
#' intersect within a test area? (The hailstones themselves don\'t have to
#' collide, just test for intersections between the paths they will trace.)
#' 
#' In this example, look for intersections that happen with an X and Y
#' position each at least `7` and at most `27`; in your actual data,
#' you\'ll need to check a much larger test area. Comparing all pairs of
#' hailstones\' future paths produces the following results:
#' 
#'     Hailstone A: 19, 13, 30 @ -2, 1, -2
#'     Hailstone B: 18, 19, 22 @ -1, -1, -2
#'     Hailstones' paths will cross inside the test area (at x=14.333, y=15.333).
#' 
#'     Hailstone A: 19, 13, 30 @ -2, 1, -2
#'     Hailstone B: 20, 25, 34 @ -2, -2, -4
#'     Hailstones' paths will cross inside the test area (at x=11.667, y=16.667).
#' 
#'     Hailstone A: 19, 13, 30 @ -2, 1, -2
#'     Hailstone B: 12, 31, 28 @ -1, -2, -1
#'     Hailstones' paths will cross outside the test area (at x=6.2, y=19.4).
#' 
#'     Hailstone A: 19, 13, 30 @ -2, 1, -2
#'     Hailstone B: 20, 19, 15 @ 1, -5, -3
#'     Hailstones' paths crossed in the past for hailstone A.
#' 
#'     Hailstone A: 18, 19, 22 @ -1, -1, -2
#'     Hailstone B: 20, 25, 34 @ -2, -2, -4
#'     Hailstones' paths are parallel; they never intersect.
#' 
#'     Hailstone A: 18, 19, 22 @ -1, -1, -2
#'     Hailstone B: 12, 31, 28 @ -1, -2, -1
#'     Hailstones' paths will cross outside the test area (at x=-6, y=-5).
#' 
#'     Hailstone A: 18, 19, 22 @ -1, -1, -2
#'     Hailstone B: 20, 19, 15 @ 1, -5, -3
#'     Hailstones' paths crossed in the past for both hailstones.
#' 
#'     Hailstone A: 20, 25, 34 @ -2, -2, -4
#'     Hailstone B: 12, 31, 28 @ -1, -2, -1
#'     Hailstones' paths will cross outside the test area (at x=-2, y=3).
#' 
#'     Hailstone A: 20, 25, 34 @ -2, -2, -4
#'     Hailstone B: 20, 19, 15 @ 1, -5, -3
#'     Hailstones' paths crossed in the past for hailstone B.
#' 
#'     Hailstone A: 12, 31, 28 @ -1, -2, -1
#'     Hailstone B: 20, 19, 15 @ 1, -5, -3
#'     Hailstones' paths crossed in the past for both hailstones.
#' 
#' So, in this example, *`2`* hailstones\' future paths cross inside the
#' boundaries of the test area.
#' 
#' However, you\'ll need to search a much larger test area if you want to
#' see if any hailstones might collide. Look for intersections that happen
#' with an X and Y position each at least `200000000000000` and at most
#' `400000000000000`. Disregard the Z axis entirely.
#' 
#' Considering only the X and Y axes, check all pairs of hailstones\'
#' future paths for intersections. *How many of these intersections occur
#' within the test area?*
#'
#' **Part Two**
#' 
#' Upon further analysis, it doesn\'t seem like *any* hailstones will
#' naturally collide. It\'s up to you to fix that!
#' 
#' You find a rock on the ground nearby. While it seems extremely unlikely,
#' if you throw it just right, you should be able to *hit every hailstone
#' in a single throw*!
#' 
#' You can use the probably-magical winds to reach *any integer position*
#' you like and to propel the rock at *any integer velocity*. Now
#' *including the Z axis* in your calculations, if you throw the rock at
#' time `0`, where do you need to be so that the rock *perfectly collides
#' with every hailstone*? Due to [probably-magical
#' inertia]{title="What, you've never studied probably-magical physics?"},
#' the rock won\'t slow down or change direction when it collides with a
#' hailstone.
#' 
#' In the example above, you can achieve this by moving to position
#' `24, 13, 10` and throwing the rock at velocity `-3, 1, 2`. If you do
#' this, you will hit every hailstone as follows:
#' 
#'     Hailstone: 19, 13, 30 @ -2, 1, -2
#'     Collision time: 5
#'     Collision position: 9, 18, 20
#' 
#'     Hailstone: 18, 19, 22 @ -1, -1, -2
#'     Collision time: 3
#'     Collision position: 15, 16, 16
#' 
#'     Hailstone: 20, 25, 34 @ -2, -2, -4
#'     Collision time: 4
#'     Collision position: 12, 17, 18
#' 
#'     Hailstone: 12, 31, 28 @ -1, -2, -1
#'     Collision time: 6
#'     Collision position: 6, 19, 22
#' 
#'     Hailstone: 20, 19, 15 @ 1, -5, -3
#'     Collision time: 1
#'     Collision position: 21, 14, 12
#' 
#' Above, each hailstone is identified by its initial position and its
#' velocity. Then, the time and position of that hailstone\'s collision
#' with your rock are given.
#' 
#' After 1 nanosecond, the rock has *exactly the same position* as one of
#' the hailstones, obliterating it into ice dust! Another hailstone is
#' smashed to bits two nanoseconds after that. After a total of 6
#' nanoseconds, all of the hailstones have been destroyed.
#' 
#' So, at time `0`, the rock needs to be at X position `24`, Y position
#' `13`, and Z position `10`. Adding these three coordinates together
#' produces *`47`*. (Don\'t add any coordinates from the rock\'s velocity.)
#' 
#' Determine the exact position and velocity the rock needs to have at time
#' `0` so that it perfectly collides with every hailstone. *What do you get
#' if you add up the X, Y, and Z coordinates of that initial position?*
#'
#' @param x some data
#' @return For Part One, `f24a(x)` returns .... For Part Two,
#'   `f24b(x)` returns ....
#' @export
#' @examples
#' f24a(example_data_24())
#' f24b()
f24a <- function(x, pt1, pt2) {
  pts <- lapply(x, f24_helper)
  
  ans <- 0
  for (i in seq_along(pts)) {
    pti <- pts[[i]]
    posi <- pti$pos
    veli <- pti$vel
    for (j in i:length(pts)) {
      if (i == j) next
      ptj <- pts[[j]]
      posj <- ptj$pos
      velj <- ptj$vel
      a_line <- sf::st_linestring(as.matrix(data.frame(x = posi[1]+veli[1]*c(0, 1e15), y = posi[2]+veli[2]*c(0, 1e15))))
      b_line <- sf::st_linestring(as.matrix(data.frame(x = posj[1]+velj[1]*c(0, 1e15), y = posj[2]+velj[2]*c(0, 1e15))))
      cross <- sf::st_intersection(a_line, b_line)
      if (length(cross) == 0) next
      if (cross[1] >= pt1 & cross[1] <= pt2 &
        cross[2] >= pt1 & cross[2] <= pt2) ans <- ans + 1
    }
  }
  ans
}

#' @rdname day24
#' @export
f24b <- function(x) {
  pts <- lapply(x, f24_helper)
  same_vel <- vector("list", 3)
  same_vel[[1]] <- data.frame(i = integer(), j = integer())
  same_vel[[2]] <- data.frame(i = integer(), j = integer())
  same_vel[[3]] <- data.frame(i = integer(), j = integer())
  for (i in seq_along(pts)) {
    for (j in i:length(pts)) {
      if (i == j) next
      if (pts[[i]]$vel[1] == pts[[j]]$vel[1]) same_vel[[1]] <- rbind(same_vel[[1]], data.frame(i, j))
      if (pts[[i]]$vel[2] == pts[[j]]$vel[2]) same_vel[[2]] <- rbind(same_vel[[2]], data.frame(i, j))
      if (pts[[i]]$vel[3] == pts[[j]]$vel[3]) same_vel[[3]] <- rbind(same_vel[[3]], data.frame(i, j))
    }
  }
  rockvel <- sapply(1:3, \(w) find_modulo(same_vel, w, pts))

  slope_a <- (pts[[1]]$vel[2] - rockvel[2])/(pts[[1]]$vel[1] - rockvel[1])
  slope_b <- (pts[[2]]$vel[2] - rockvel[2])/(pts[[2]]$vel[1] - rockvel[1])
  ca <- pts[[1]]$pos[2] - (slope_a * pts[[1]]$pos[1])
  cb <- pts[[2]]$pos[2] - (slope_b * pts[[2]]$pos[1])
  xpos <- (cb - ca)/(slope_a - slope_b)
  ypos <- slope_a * xpos + ca
  t <- (xpos - pts[[1]]$pos[1]) / (pts[[1]]$vel[1] - rockvel[1])
  zpos <- pts[[1]]$pos[3] + (pts[[1]]$vel[3] - rockvel[3])*t

  sprintf("%.15g", sum(c(xpos, ypos, zpos)))
}

find_modulo <- function(s, i, p) {
  rockvel <- c()
  for (j in seq_len(nrow(s[[i]]))) {
    potv <- c()
    if (abs(p[[s[[i]][j,1]]]$vel[i]) < 100) next
    difference <- p[[s[[i]][j,2]]]$pos[i] - p[[s[[i]][j,1]]]$pos[i]
    for (v in -1000:1000) {
      test <- difference %% (v - p[[s[[i]][j,1]]]$vel[i])
      if (is.finite(test) && test == 0) {
        potv <- c(potv, v)
      }
    }
    if (length(rockvel) > 0) {
      rockvel <- intersect(rockvel, potv)
    } else {
      rockvel <- potv
    }
  }
  rockvel
}

f24_helper <- function(x) {
  x <- strsplit(x, "[:,@]")[[1]]
  list(pos = as.numeric(x[1:3]), vel = as.numeric(x[4:6]))
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day24
#' @export
example_data_24 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
