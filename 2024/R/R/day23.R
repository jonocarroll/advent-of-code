#' Day 23: LAN Party
#'
#' [LAN Party](https://adventofcode.com/2024/day/23)
#'
#' @name day23
#' @rdname day23
#' @details
#'
#' **Part One**
#'
#' As The Historians wander around a secure area at Easter Bunny HQ, you
#' come across posters for a [LAN
#' party](https://en.wikipedia.org/wiki/LAN_party){target="_blank"}
#' scheduled for today! Maybe you can find it; you connect to a nearby
#' [datalink port](/2016/day/9) and download a map of the local network
#' (your puzzle input).
#' 
#' The network map provides a list of every *connection between two
#' computers*. For example:
#' 
#'     kh-tc
#'     qp-kh
#'     de-cg
#'     ka-co
#'     yn-aq
#'     qp-ub
#'     cg-tb
#'     vc-aq
#'     tb-ka
#'     wh-tc
#'     yn-cg
#'     kh-ub
#'     ta-co
#'     de-co
#'     tc-td
#'     tb-wq
#'     wh-td
#'     ta-ka
#'     td-qp
#'     aq-cg
#'     wq-ub
#'     ub-vc
#'     de-ta
#'     wq-aq
#'     wq-vc
#'     wh-yn
#'     ka-de
#'     kh-ta
#'     co-tc
#'     wh-qp
#'     tb-vc
#'     td-yn
#' 
#' Each line of text in the network map represents a single connection; the
#' line `kh-tc` represents a connection between the computer named `kh` and
#' the computer named `tc`. Connections aren\'t directional; `tc-kh` would
#' mean exactly the same thing.
#' 
#' LAN parties typically involve multiplayer games, so maybe you can locate
#' it by finding groups of connected computers. Start by looking for *sets
#' of three computers* where each computer in the set is connected to the
#' other two computers.
#' 
#' In this example, there are `12` such sets of three inter-connected
#' computers:
#' 
#'     aq,cg,yn
#'     aq,vc,wq
#'     co,de,ka
#'     co,de,ta
#'     co,ka,ta
#'     de,ka,ta
#'     kh,qp,ub
#'     qp,td,wh
#'     tb,vc,wq
#'     tc,td,wh
#'     td,wh,yn
#'     ub,vc,wq
#' 
#' If the Chief Historian is here, *and* he\'s at the LAN party, it would
#' be best to know that right away. You\'re pretty sure his computer\'s
#' name starts with `t`, so consider only sets of three computers where at
#' least one computer\'s name starts with `t`. That narrows the list down
#' to *`7`* sets of three inter-connected computers:
#' 
#'     co,de,ta
#'     co,ka,ta
#'     de,ka,ta
#'     qp,td,wh
#'     tb,vc,wq
#'     tc,td,wh
#'     td,wh,yn
#' 
#' Find all the sets of three inter-connected computers. *How many contain
#' at least one computer with a name that starts with `t`?*
#'
#' **Part Two**
#' 
#' There are still way too many results to go through them all. You\'ll
#' have to find the LAN party another way and go there yourself.
#' 
#' Since it doesn\'t seem like any employees are around, you figure they
#' must all be at the LAN party. If that\'s true, the LAN party will be the
#' *largest set of computers that are all connected to each other*. That
#' is, for each computer at the LAN party, that computer will have a
#' connection to every other computer at the LAN party.
#' 
#' In the above example, the largest set of computers that are all
#' connected to each other is made up of `co`, `de`, `ka`, and `ta`. Each
#' computer in this set has a connection to every other computer in the
#' set:
#' 
#'     ka-co
#'     ta-co
#'     de-co
#'     ta-ka
#'     de-ta
#'     ka-de
#' 
#' The LAN party posters say that the *password* to get into the LAN party
#' is the name of every computer at the LAN party, sorted alphabetically,
#' then joined together with commas. (The people running the LAN party are
#' clearly a bunch of [nerds]{title="You caught me. I'm a giant nerd."}.)
#' In this example, the password would be *`co,de,ka,ta`*.
#' 
#' *What is the password to get into the LAN party?*
#'
#' @param x some data
#' @return For Part One, `f23a(x)` returns .... For Part Two,
#'   `f23b(x)` returns ....
#' @export
#' @examples
#' f23a(example_data_23())
#' f23b()
f23a <- function(x) {
  x <- readLines("../tmp3.txt")
  x <- readLines("inst/input23.txt")
  conns <- as.data.frame(do.call(rbind, strsplit(x, "-")))
  library(igraph)
  g <- graph_from_data_frame(conns, directed = FALSE)
  allv <- V(g)
  ts <- allv[startsWith(names(allv), "t")]
  # list_triangles(g, "td")
  tri <- triangles(g)
  verts <- lapply(split(tri, gl(length(tri)/3, 3)), \(z) names(unclass(z)))
  tverts <- Filter(\(z) any(startsWith(z, "t")), verts)
  length(tverts) # part 1
  
  lc <- igraph::largest_cliques(g)[[1]]
  paste(sort(names(lc)), collapse = ",") # part 2
}


#' @rdname day23
#' @export
f23b <- function(x) {

}


f23_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day23
#' @export
example_data_23 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
