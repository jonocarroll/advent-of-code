#' Day 08: Playground
#'
#' [Playground](https://adventofcode.com/2025/day/8)
#'
#' @name day08
#' @rdname day08
#' @details
#'
#' **Part One**
#'
#' Equipped with a new understanding of teleporter maintenance, you
#' confidently step onto the repaired teleporter pad.
#'
#' You rematerialize on an unfamiliar teleporter pad and find yourself in a
#' vast underground space which contains a giant playground!
#'
#' Across the playground, a group of Elves are working on setting up an
#' ambitious Christmas decoration project. Through careful rigging, they
#' have suspended a large number of small electrical [junction
#' boxes](https://en.wikipedia.org/wiki/Junction_box){target="_blank"}.
#'
#' Their plan is to connect the junction boxes with long strings of lights.
#' Most of the junction boxes don\'t provide electricity; however, when two
#' junction boxes are connected by a string of lights, electricity can pass
#' between those two junction boxes.
#'
#' The Elves are trying to figure out *which junction boxes to connect* so
#' that electricity can reach *every* junction box. They even have a list
#' of all of the junction boxes\' positions in 3D space (your puzzle
#' input).
#'
#' For example:
#'
#'     162,817,812
#'     57,618,57
#'     906,360,560
#'     592,479,940
#'     352,342,300
#'     466,668,158
#'     542,29,236
#'     431,825,988
#'     739,650,466
#'     52,470,668
#'     216,146,977
#'     819,987,18
#'     117,168,530
#'     805,96,715
#'     346,949,466
#'     970,615,88
#'     941,993,340
#'     862,61,35
#'     984,92,344
#'     425,690,689
#'
#' This list describes the position of 20 junction boxes, one per line.
#' Each position is given as `X,Y,Z` coordinates. So, the first junction
#' box in the list is at `X=162`, `Y=817`, `Z=812`.
#'
#' To save on string lights, the Elves would like to focus on connecting
#' pairs of junction boxes that are *as close together as possible*
#' according to [straight-line
#' distance](https://en.wikipedia.org/wiki/Euclidean_distance){target="_blank"}.
#' In this example, the two junction boxes which are closest together are
#' `162,817,812` and `425,690,689`.
#'
#' By connecting these two junction boxes together, because electricity can
#' flow between them, they become part of the same *circuit*. After
#' connecting them, there is a single circuit which contains two junction
#' boxes, and the remaining 18 junction boxes remain in their own
#' individual circuits.
#'
#' Now, the two junction boxes which are closest together but aren\'t
#' already directly connected are `162,817,812` and `431,825,988`. After
#' connecting them, since `162,817,812` is already connected to another
#' junction box, there is now a single circuit which contains *three*
#' junction boxes and an additional 17 circuits which contain one junction
#' box each.
#'
#' The next two junction boxes to connect are `906,360,560` and
#' `805,96,715`. After connecting them, there is a circuit containing 3
#' junction boxes, a circuit containing 2 junction boxes, and 15 circuits
#' which contain one junction box each.
#'
#' The next two junction boxes are `431,825,988` and `425,690,689`. Because
#' these two junction boxes were *already in the same circuit*, nothing
#' happens!
#'
#' This process continues for a while, and the Elves are concerned that
#' they don\'t have enough extension cables for all these circuits. They
#' would like to know how big the circuits will be.
#'
#' After making the ten shortest connections, there are 11 circuits: one
#' circuit which contains *5* junction boxes, one circuit which contains
#' *4* junction boxes, two circuits which contain *2* junction boxes each,
#' and seven circuits which each contain a single junction box. Multiplying
#' together the sizes of the three largest circuits (5, 4, and one of the
#' circuits of size 2) produces *`40`*.
#'
#' Your list contains many junction boxes; connect together the *1000*
#' pairs of junction boxes which are closest together. Afterward, *what do
#' you get if you multiply together the sizes of the three largest
#' circuits?*
#'
#' **Part Two**
#'
#' The Elves were right; they *definitely* don\'t have enough extension
#' cables. You\'ll need to keep connecting junction boxes together until
#' they\'re all in *one large circuit*.
#'
#' Continuing the above example, the first connection which causes all of
#' the junction boxes to form a single circuit is between the junction
#' boxes at `216,146,977` and `117,168,530`. The Elves need to know how far
#' those junction boxes are from the wall so they can pick the right
#' extension cable; multiplying the X coordinates of those two junction
#' boxes (`216` and `117`) produces *`25272`*.
#'
#' Continue connecting the closest unconnected pairs of junction boxes
#' together until they\'re [all in the same
#' circuit]{title="I strongly recommend making an interactive visualizer for this one; it reminds me a lot of maps from futuristic space games."}.
#' *What do you get if you multiply together the X coordinates of the last
#' two junction boxes you need to connect?*
#'
#' @param x some data
#' @return For Part One, `f08a(x)` returns .... For Part Two,
#'   `f08b(x)` returns ....
#' @export
#' @examples
#' f08a(example_data_08())
#' f08b()
f08a <- function(x) {
  # junctions <- read.csv("input.txt", header = FALSE)
  junctions <- read.csv("inst/input08.txt", header = FALSE)
  n <- 1000
  circuits <- list()
  dists <- outer(seq_len(nrow(junctions)), seq_len(nrow(junctions)), mindist)
  distsnz <- dists[dists > 0]
  lens <- sort(unique(distsnz))
  i <- 1
  while (i <= n) {
    closest <- which(dists == lens[i], arr.ind = TRUE)
    closest <- sort(closest[closest[,1] > closest[,2]])
    if (any(sapply(circuits, \(x) all(as.list(sort(closest)) == x)))) {
      message("already connected")
      next
    }
    circuits <- c(circuits, list(closest))
    i <- i + 1
  }
  groups <- dset(circuits)
  prod(head(rev(sort(lengths(groups))), 3))
}

mindist <- Vectorize(function(p1, p2) {
  sqrt(sum((junctions[p2, ] - junctions[p1, ])^2))
})


# https://stackoverflow.com/questions/25130462/get-disjoint-sets-from-a-list-in-r
dset <- function(lst) {
  Reduce(function(l, x)  {
    matches <- sapply(l, function(i) any(x %in% i))

    if (any(matches)) {
      combined <- unique(c(unlist(l[matches]), x))
      l[matches] <- NULL        # Delete old entries
      l <- c(l, list(combined)) # Add combined entries
    } else {
      l <- c(l, list(x))        # New list entry
    }
    l
  }, lst, init=list())
}

#' @rdname day08
#' @export
f08b <- function(x) {
  # junctions <- read.csv("input.txt", header = FALSE)
  junctions <- read.csv("inst/input08.txt", header = FALSE)
  circuits <- list()
  dists <- outer(seq_len(nrow(junctions)), seq_len(nrow(junctions)), mindist)
  distsnz <- dists[dists > 0]
  lens <- sort(unique(distsnz))
  i <- 1
  allcon <- FALSE
  while (!allcon) {
    closest <- which(dists == lens[i], arr.ind = TRUE)
    closest <- sort(closest[closest[,1] > closest[,2]])
    circuits <- c(circuits, list(closest))
    groups <- dset(circuits)
    allcon <- all(seq_len(nrow(junctions)) %in% groups[[1]])
    i <- i + 1
  }
  last <- circuits[[length(circuits)]]
  junctions[last[1], 1] * junctions[last[2], 1]
}


f08_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day08
#' @export
example_data_08 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
