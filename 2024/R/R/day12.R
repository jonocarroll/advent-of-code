#' Day 12: Garden Groups
#'
#' [Garden Groups](https://adventofcode.com/2024/day/12)
#'
#' @name day12
#' @rdname day12
#' @details
#'
#' **Part One**
#'
#' Why not search for the Chief Historian near the [gardener](/2023/day/5)
#' and his [massive farm](/2023/day/21)? There\'s plenty of food, so The
#' Historians grab something to eat while they search.
#' 
#' You\'re about to settle near a complex arrangement of garden plots when
#' some Elves ask if you can lend a hand. They\'d like to set up
#' [fences]{title="I originally wanted to title this puzzle \"Fencepost Problem\", but I was afraid someone would then try to count fenceposts by mistake and experience a fencepost problem."}
#' around each region of garden plots, but they can\'t figure out how much
#' fence they need to order or how much it will cost. They hand you a map
#' (your puzzle input) of the garden plots.
#' 
#' Each garden plot grows only a single type of plant and is indicated by a
#' single letter on your map. When multiple garden plots are growing the
#' same type of plant and are touching (horizontally or vertically), they
#' form a *region*. For example:
#' 
#'     AAAA
#'     BBCD
#'     BBCC
#'     EEEC
#' 
#' This 4x4 arrangement includes garden plots growing five different types
#' of plants (labeled `A`, `B`, `C`, `D`, and `E`), each grouped into their
#' own region.
#' 
#' In order to accurately calculate the cost of the fence around a single
#' region, you need to know that region\'s *area* and *perimeter*.
#' 
#' The *area* of a region is simply the number of garden plots the region
#' contains. The above map\'s type `A`, `B`, and `C` plants are each in a
#' region of area `4`. The type `E` plants are in a region of area `3`; the
#' type `D` plants are in a region of area `1`.
#' 
#' Each garden plot is a square and so has *four sides*. The *perimeter* of
#' a region is the number of sides of garden plots in the region that do
#' not touch another garden plot in the same region. The type `A` and `C`
#' plants are each in a region with perimeter `10`. The type `B` and `E`
#' plants are each in a region with perimeter `8`. The lone `D` plot forms
#' its own region with perimeter `4`.
#' 
#' Visually indicating the sides of plots in each region that contribute to
#' the perimeter using `-` and `|`, the above map\'s regions\' perimeters
#' are measured as follows:
#' 
#'     +-+-+-+-+
#'     |A A A A|
#'     +-+-+-+-+     +-+
#'                   |D|
#'     +-+-+   +-+   +-+
#'     |B B|   |C|
#'     +   +   + +-+
#'     |B B|   |C C|
#'     +-+-+   +-+ +
#'               |C|
#'     +-+-+-+   +-+
#'     |E E E|
#'     +-+-+-+
#' 
#' Plants of the same type can appear in multiple separate regions, and
#' regions can even appear within other regions. For example:
#' 
#'     OOOOO
#'     OXOXO
#'     OOOOO
#'     OXOXO
#'     OOOOO
#' 
#' The above map contains *five* regions, one containing all of the `O`
#' garden plots, and the other four each containing a single `X` plot.
#' 
#' The four `X` regions each have area `1` and perimeter `4`. The region
#' containing `21` type `O` plants is more complicated; in addition to its
#' outer edge contributing a perimeter of `20`, its boundary with each `X`
#' region contributes an additional `4` to its perimeter, for a total
#' perimeter of `36`.
#' 
#' Due to \"modern\" business practices, the *price* of fence required for
#' a region is found by *multiplying* that region\'s area by its perimeter.
#' The *total price* of fencing all regions on a map is found by adding
#' together the price of fence for every region on the map.
#' 
#' In the first example, region `A` has price `4 * 10 = 40`, region `B` has
#' price `4 * 8 = 32`, region `C` has price `4 * 10 = 40`, region `D` has
#' price `1 * 4 = 4`, and region `E` has price `3 * 8 = 24`. So, the total
#' price for the first example is *`140`*.
#' 
#' In the second example, the region with all of the `O` plants has price
#' `21 * 36 = 756`, and each of the four smaller `X` regions has price
#' `1 * 4 = 4`, for a total price of *`772`* (`756 + 4 + 4 + 4 + 4`).
#' 
#' Here\'s a larger example:
#' 
#'     RRRRIICCFF
#'     RRRRIICCCF
#'     VVRRRCCFFF
#'     VVRCCCJFFF
#'     VVVVCJJCFE
#'     VVIVCCJJEE
#'     VVIIICJJEE
#'     MIIIIIJJEE
#'     MIIISIJEEE
#'     MMMISSJEEE
#' 
#' It contains:
#' 
#' -   A region of `R` plants with price `12 * 18 = 216`.
#' -   A region of `I` plants with price `4 * 8 = 32`.
#' -   A region of `C` plants with price `14 * 28 = 392`.
#' -   A region of `F` plants with price `10 * 18 = 180`.
#' -   A region of `V` plants with price `13 * 20 = 260`.
#' -   A region of `J` plants with price `11 * 20 = 220`.
#' -   A region of `C` plants with price `1 * 4 = 4`.
#' -   A region of `E` plants with price `13 * 18 = 234`.
#' -   A region of `I` plants with price `14 * 22 = 308`.
#' -   A region of `M` plants with price `5 * 12 = 60`.
#' -   A region of `S` plants with price `3 * 8 = 24`.
#' 
#' So, it has a total price of *`1930`*.
#' 
#' *What is the total price of fencing all regions on your map?*
#'
#' **Part Two**
#' 
#' Fortunately, the Elves are trying to order so much fence that they
#' qualify for a *bulk discount*!
#' 
#' Under the bulk discount, instead of using the perimeter to calculate the
#' price, you need to use the *number of sides* each region has. Each
#' straight section of fence counts as a side, regardless of how long it
#' is.
#' 
#' Consider this example again:
#' 
#'     AAAA
#'     BBCD
#'     BBCC
#'     EEEC
#' 
#' The region containing type `A` plants has `4` sides, as does each of the
#' regions containing plants of type `B`, `D`, and `E`. However, the more
#' complex region containing the plants of type `C` has `8` sides!
#' 
#' Using the new method of calculating the per-region price by multiplying
#' the region\'s area by its number of sides, regions `A` through `E` have
#' prices `16`, `16`, `32`, `4`, and `12`, respectively, for a total price
#' of *`80`*.
#' 
#' The second example above (full of type `X` and `O` plants) would have a
#' total price of *`436`*.
#' 
#' Here\'s a map that includes an E-shaped region full of type `E` plants:
#' 
#'     EEEEE
#'     EXXXX
#'     EEEEE
#'     EXXXX
#'     EEEEE
#' 
#' The E-shaped region has an area of `17` and `12` sides for a price of
#' `204`. Including the two regions full of type `X` plants, this map has a
#' total price of *`236`*.
#' 
#' This map has a total price of *`368`*:
#' 
#'     AAAAAA
#'     AAABBA
#'     AAABBA
#'     ABBAAA
#'     ABBAAA
#'     AAAAAA
#' 
#' It includes two regions full of type `B` plants (each with `4` sides)
#' and a single region full of type `A` plants (with `4` sides on the
#' outside and `8` more sides on the inside, a total of `12` sides). Be
#' especially careful when counting the fence around regions like the one
#' full of type `A` plants; in particular, each section of fence has an
#' in-side and an out-side, so the fence does not connect across the middle
#' of the region (where the two `B` regions touch diagonally). (The Elves
#' would have used the Möbius Fencing Company instead, but their contract
#' terms were too one-sided.)
#' 
#' The larger example from before now has the following updated prices:
#' 
#' -   A region of `R` plants with price `12 * 10 = 120`.
#' -   A region of `I` plants with price `4 * 4 = 16`.
#' -   A region of `C` plants with price `14 * 22 = 308`.
#' -   A region of `F` plants with price `10 * 12 = 120`.
#' -   A region of `V` plants with price `13 * 10 = 130`.
#' -   A region of `J` plants with price `11 * 12 = 132`.
#' -   A region of `C` plants with price `1 * 4 = 4`.
#' -   A region of `E` plants with price `13 * 8 = 104`.
#' -   A region of `I` plants with price `14 * 16 = 224`.
#' -   A region of `M` plants with price `5 * 6 = 30`.
#' -   A region of `S` plants with price `3 * 6 = 18`.
#' 
#' Adding these together produces its new total price of *`1206`*.
#' 
#' *What is the new total price of fencing all regions on your map?*
#'
#' @param x some data
#' @return For Part One, `f12a(x)` returns .... For Part Two,
#'   `f12b(x)` returns ....
#' @export
#' @examples
#' f12a(example_data_12())
#' f12b()
f12a <- function(x) {
  # x <- readLines("../tmp.txt")
  x <- readLines("inst/input12.txt")
  x <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
    
  ids <- matrix(1:prod(dim(x)), nrow = nrow(x), byrow = TRUE)
  plots <- matrix(0, nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
  
  assigned <- c()
  next_search <- 1
  grp <- 1
  while (length(next_search)) {
    visited <<- c()
    this_grp <- c(next_search, flood(next_search, c()))
    plots[which(ids %in% this_grp, arr.ind = TRUE)] <- grp
    grp <- grp + 1
    assigned <- c(assigned, this_grp)
    next_search <- head(setdiff(1:prod(dim(x)), assigned), 1)
  }
  
  sum(sapply(unique(c(plots)), ap, plots))
  
}

ap <- function(x, p) {
  locs <- which(x == p, arr.ind = T)
  clocs <- complex(real = locs[, 1], imaginary = locs[, 2])
  b <- 0
  for (r in seq_along(clocs)) {
    b <- b + sum(!(clocs[r] + c(1+0i, -1+0i, 0+1i, 0-1i)) %in% clocs)
  }
  length(clocs) * b
}

flood <- function(p, in_grp) {
  pos <- get_pos(x, p)
  i <- pos[1]; j <- pos[2]
  visited <<- c(visited, p)
  for (n in setdiff(nbrs(p), visited)) {
    nextpos <- which(ids == n, arr.ind = T)
    if (x[nextpos[1], nextpos[2]] == x[i, j]) {
      in_grp <- c(n, flood(n, in_grp))
    } 
  }
  in_grp
}

nbrs <- function(p) {
  pos <- get_pos(x, p)
  i <- pos[1]; j <- pos[2]
  dirs <- list(c(i-1,j), c(i+1,j), c(i,j-1), c(i,j+1))
  unlist(sapply(dirs, \(z) { 
    if (inbounds(x, z[1], z[2])) ids[z[1], z[2]] 
  }))
}

get_pos <- function(grid, v) {
  i <- floor((v-1)/nrow(grid))+1
  j <- ((v-1) %% nrow(grid))+1
  return(c(i, j))
}

inbounds <- function(z, x, y) {
  x > 0 & y > 0 & x <= nrow(z) & y <= ncol(z)
}


#' @rdname day12
#' @export
f12b <- function(x) {
  # x <- readLines("../tmp.txt")
  x <- readLines("inst/input12.txt")
  x <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  
  ids <- matrix(1:prod(dim(x)), nrow = nrow(x), byrow = TRUE)
  plots <- matrix(0, nrow = nrow(x), ncol = ncol(x), byrow = TRUE)
  
  assigned <- c()
  next_search <- 1
  grp <- 1
  while (length(next_search)) {
    visited <<- c()
    this_grp <- c(next_search, grow(next_search, c()))
    plots[which(ids %in% this_grp, arr.ind = TRUE)] <- grp
    grp <- grp + 1
    assigned <- c(assigned, this_grp)
    next_search <- head(setdiff(1:prod(dim(x)), assigned), 1)
  }
  
  plots <- cbind(cbind(0, rbind(rbind(0, plots), 0)), 0)
  totprice <- 0
  for (plant in setdiff(unique(c(plots)), 0)) {
    totprice <- totprice + newprice(plant)
  }
  totprice
  
}

newprice <- function(plant) {
  plist <- which(plots == plant, arr.ind = TRUE)
  sides <- 0
  area <- 0
  for (i in seq_len(nrow(plist))) {
    if (plots[plist[i, 1], plist[i, 2]] == plant) {
      area <- area + 1
    }
    sides <- sides + corners(c(plist[i, ]), c(1, 1)) + 
      corners(c(plist[i, ]), c(-1, 1)) +
      corners(c(plist[i, ]), c(1, -1)) + 
      corners(c(plist[i, ]), c(-1, -1))
  }  
  area*sides
}

corners <- function(p, d) {
  dir1 <- d[1]
  dir2 <- d[2]
  v <- plots[p[1], p[2]]
  up <- p + c(dir1, 0)
  up_v <- plots[up[1], up[2]]
  right <- p + c(0, dir2)
  right_v <- plots[right[1], right[2]]
  diag <- p + c(dir1, dir2)
  diag_v <- plots[diag[1], diag[2]]
  (((v != up_v) && (v != right_v)) || ((v == up_v) && (v == right_v) && (v != diag_v)))
}


f12_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day12
#' @export
example_data_12 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
