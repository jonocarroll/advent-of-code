#' Day 13: Point of Incidence
#'
#' [Point of Incidence](https://adventofcode.com/2023/day/13)
#'
#' @name day13
#' @rdname day13
#' @details
#'
#' **Part One**
#'
#' With your help, the hot springs team locates an appropriate spring which
#' launches you neatly and precisely up to the edge of *Lava Island*.
#' 
#' There\'s just one problem: you don\'t see any *lava*.
#' 
#' You *do* see a lot of ash and igneous rock; there are even what look
#' like gray mountains scattered around. After a while, you make your way
#' to a nearby cluster of mountains only to discover that the valley
#' between them is completely full of large *mirrors*. Most of the mirrors
#' seem to be aligned in a consistent way; perhaps you should head in that
#' direction?
#' 
#' As you move through the valley of mirrors, you find that several of them
#' have fallen from the large metal frames keeping them in place. The
#' mirrors are extremely flat and shiny, and many of the fallen mirrors
#' have lodged into the ash at strange angles. Because the terrain is all
#' one color, it\'s hard to tell where it\'s safe to walk or where you\'re
#' about to run into a mirror.
#' 
#' You note down the patterns of ash (`.`) and rocks (`#`) that you see as
#' you walk (your puzzle input); perhaps by carefully analyzing these
#' patterns, you can figure out where the mirrors are!
#' 
#' For example:
#' 
#'     #.##..##.
#'     ..#.##.#.
#'     ##......#
#'     ##......#
#'     ..#.##.#.
#'     ..##..##.
#'     #.#.##.#.
#' 
#'     #...##..#
#'     #....#..#
#'     ..##..###
#'     #####.##.
#'     #####.##.
#'     ..##..###
#'     #....#..#
#' 
#' To find the reflection in each pattern, you need to find a perfect
#' reflection across either a horizontal line between two rows or across a
#' vertical line between two columns.
#' 
#' In the first pattern, the reflection is across a vertical line between
#' two columns; arrows on each of the two columns point at the line between
#' the columns:
#' 
#'     123456789
#'         ><   
#'     #.##..##.
#'     ..#.##.#.
#'     ##......#
#'     ##......#
#'     ..#.##.#.
#'     ..##..##.
#'     #.#.##.#.
#'         ><   
#'     123456789
#' 
#' In this pattern, the line of reflection is the vertical line between
#' columns 5 and 6. Because the vertical line is not perfectly in the
#' middle of the pattern, part of the pattern (column 1) has nowhere to
#' reflect onto and can be ignored; every other column has a reflected
#' column within the pattern and must match exactly: column 2 matches
#' column 9, column 3 matches 8, 4 matches 7, and 5 matches 6.
#' 
#' The second pattern reflects across a horizontal line instead:
#' 
#'     1 #...##..# 1
#'     2 #....#..# 2
#'     3 ..##..### 3
#'     4v#####.##.v4
#'     5^#####.##.^5
#'     6 ..##..### 6
#'     7 #....#..# 7
#' 
#' This pattern reflects across the horizontal line between rows 4 and 5.
#' Row 1 would reflect with a hypothetical row 8, but since that\'s not in
#' the pattern, row 1 doesn\'t need to match anything. The remaining rows
#' match: row 2 matches row 7, row 3 matches row 6, and row 4 matches row
#' 5.
#' 
#' To *summarize* your pattern notes, add up *the number of columns* to the
#' left of each vertical line of reflection; to that, also add *100
#' multiplied by the number of rows* above each horizontal line of
#' reflection. In the above example, the first pattern\'s vertical line has
#' `5` columns to its left and the second pattern\'s horizontal line has
#' `4` rows above it, a total of *`405`*.
#' 
#' Find the line of reflection in each of the patterns in your notes. *What
#' number do you get after summarizing all of your notes?*
#'
#' **Part Two**
#' 
#' You resume walking through the valley of mirrors and - *SMACK!* - run
#' directly into one. Hopefully [nobody]{title="Sorry, Nobody saw that."}
#' was watching, because that must have been pretty embarrassing.
#' 
#' Upon closer inspection, you discover that every mirror has exactly one
#' *smudge*: exactly one `.` or `#` should be the opposite type.
#' 
#' In each pattern, you\'ll need to locate and fix the smudge that causes a
#' *different reflection line* to be valid. (The old reflection line won\'t
#' necessarily continue being valid after the smudge is fixed.)
#' 
#' Here\'s the above example again:
#' 
#'     #.##..##.
#'     ..#.##.#.
#'     ##......#
#'     ##......#
#'     ..#.##.#.
#'     ..##..##.
#'     #.#.##.#.
#' 
#'     #...##..#
#'     #....#..#
#'     ..##..###
#'     #####.##.
#'     #####.##.
#'     ..##..###
#'     #....#..#
#' 
#' The first pattern\'s smudge is in the top-left corner. If the top-left
#' `#` were instead `.`, it would have a different, horizontal line of
#' reflection:
#' 
#'     1 ..##..##. 1
#'     2 ..#.##.#. 2
#'     3v##......#v3
#'     4^##......#^4
#'     5 ..#.##.#. 5
#'     6 ..##..##. 6
#'     7 #.#.##.#. 7
#' 
#' With the smudge in the top-left corner repaired, a new horizontal line
#' of reflection between rows 3 and 4 now exists. Row 7 has no
#' corresponding reflected row and can be ignored, but every other row
#' matches exactly: row 1 matches row 6, row 2 matches row 5, and row 3
#' matches row 4.
#' 
#' In the second pattern, the smudge can be fixed by changing the fifth
#' symbol on row 2 from `.` to `#`:
#' 
#'     1v#...##..#v1
#'     2^#...##..#^2
#'     3 ..##..### 3
#'     4 #####.##. 4
#'     5 #####.##. 5
#'     6 ..##..### 6
#'     7 #....#..# 7
#' 
#' Now, the pattern has a different horizontal line of reflection between
#' rows 1 and 2.
#' 
#' Summarize your notes as before, but instead use the new different
#' reflection lines. In this example, the first pattern\'s new horizontal
#' line has 3 rows above it and the second pattern\'s new horizontal line
#' has 1 row above it, summarizing to the value *`400`*.
#' 
#' In each pattern, fix the smudge and find the different line of
#' reflection. *What number do you get after summarizing the new reflection
#' line in each pattern in your notes?*
#'
#' @param x some data
#' @return For Part One, `f13a(x)` returns .... For Part Two,
#'   `f13b(x)` returns ....
#' @export
#' @examples
#' f13a(example_data_13())
#' f13b()
f13a <- function(x) {
  
  z <- f13_helper(x)
    
  bits <- list(
    cols = lapply(lapply(z, \(w) apply(w, 2,  GA::binary2decimal)), detect_mirror),
    rows = lapply(lapply(z, \(w) apply(w, 1,  GA::binary2decimal)), detect_mirror)
  )
  
  sum(unlist(bits$cols)) + sum(100*unlist(bits$rows))
  
}


#' @rdname day13
#' @export
f13b <- function(x) {
  
  z <- f13_helper(x)
  
  tra <- 0
  ans <- 0
  for (zz in z) {
    tra <- tra + 1
    orig_col <- detect_mirror(apply(zz, 2, GA::binary2decimal))
    orig_row <- detect_mirror(apply(zz, 1, GA::binary2decimal))
    
    refl_cols <- NULL
    refl_rows <- NULL
    
    sites <- expand.grid(1:nrow(zz), 1:ncol(zz))
    for (repl in seq_len(nrow(sites))) {
      
      tmpzz <- zz
      val <- tmpzz[sites[repl, 1], sites[repl, 2]]
      tmpzz[sites[repl, 1], sites[repl, 2]] <- ifelse(val == 1, 0, 1)
      new_col <- detect_mirror(apply(tmpzz, 2, GA::binary2decimal))
      new_row <- detect_mirror(apply(tmpzz, 1, GA::binary2decimal))
      
      refl_cols <- c(refl_cols, new_col)
      refl_rows <- c(refl_rows, new_row)
      
    }
    
    refl_rows <- setdiff(refl_rows, c(0, orig_row))
    refl_cols <- setdiff(refl_cols, c(0, orig_col))
    
    if (length(unique(refl_cols)) + length(unique(refl_rows)) == 0) stop("None")
    if (length(refl_rows) == 0) refl_rows <- 0
    if (length(refl_cols) == 0) refl_cols <- 0
    if (length(unique(refl_rows)) != 1) stop("too many rows")
    if (length(unique(refl_cols)) != 1) stop("too many cols")
    ans <- ans + unique(refl_cols) + 100*unique(refl_rows)
  }
  
  ans
  
}

f13_helper <- function(x) {
  y <- vector(mode = "list", length = length(which(x == "")) + 1)
  grp <- 1
  y[[grp]] <- list(NULL)
  for (l in x) {
    if (l == "") {
      grp <- grp + 1
      y[[grp]] <- list(NULL)
    } else {
      y[[grp]] <- c(y[[grp]], l)
    }
  }
  y <- lapply(y, unlist)
  y <- lapply(y, \(x) matrix(strsplit(paste(x, collapse = ""), "")[[1]], 
                             ncol = nchar(x[1]), 
                             byrow = TRUE))
  
  z <- lapply(y, \(w) {
    tmp <- gsub("\\.", "0", gsub("#", "1", w))
    mode(tmp) <- "integer"
    tmp
  })
  z
}


detect_mirror <- function(xx) {
  found <- 0
  for (i in 1:length(xx)) {
    mirror <- TRUE
    for (j in 1:min(length(xx) - i + 1, i - 1)) {
      mirror <- mirror && (max(0, xx[i-j]) == max(0, xx[i+j-1]))
    }
    if (mirror) {
      found <- c(found, i-1)
    }
  }
  found
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day13
#' @export
example_data_13 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
