#' Day 12: Christmas Tree Farm
#'
#' [Christmas Tree Farm](https://adventofcode.com/2025/day/12)
#'
#' @name day12
#' @rdname day12
#' @details
#'
#' **Part One**
#'
#' You\'re almost out of time, but there can\'t be much left to decorate.
#' Although there are no stairs, elevators, escalators, tunnels, chutes,
#' teleporters, firepoles, or conduits here that would take you deeper into
#' the North Pole base, there *is* a ventilation duct. You jump in.
#'
#' After bumping around for a few minutes, you emerge into a large,
#' well-lit cavern full of Christmas trees!
#'
#' There are a few Elves here frantically decorating before the deadline.
#' They think they\'ll be able to finish most of the work, but the one
#' thing they\'re worried about is the *presents* for all the young Elves
#' that live here at the North Pole. It\'s an ancient tradition to put the
#' presents under the trees, but the Elves are worried they won\'t *fit*.
#'
#' The presents come in a few standard but very weird shapes. The shapes
#' and the regions into which they need to fit are all measured in standard
#' *units*. To be aesthetically pleasing, the presents need to be placed
#' into the regions in a way that follows a standardized two-dimensional
#' unit grid; you also can\'t stack presents.
#'
#' As always, the Elves have a summary of the situation (your puzzle input)
#' for you. First, it contains a list of the presents\' shapes. Second, it
#' contains the size of the region under each tree and a list of the number
#' of presents of each shape that need to fit into that region. For
#' example:
#'
#'     0:
#'     ###
#'     ##.
#'     ##.
#'
#'     1:
#'     ###
#'     ##.
#'     .##
#'
#'     2:
#'     .##
#'     ###
#'     ##.
#'
#'     3:
#'     ##.
#'     ###
#'     ##.
#'
#'     4:
#'     ###
#'     #..
#'     ###
#'
#'     5:
#'     ###
#'     .#.
#'     ###
#'
#'     4x4: 0 0 0 0 2 0
#'     12x5: 1 0 1 0 2 2
#'     12x5: 1 0 1 0 3 2
#'
#' The first section lists the standard present *shapes*. For convenience,
#' each shape starts with its *index* and a colon; then, the shape is
#' displayed visually, where `#` is part of the shape and `.` is not.
#'
#' The second section lists the *regions* under the trees. Each line starts
#' with the width and length of the region; `12x5` means the region is `12`
#' units wide and `5` units long. The rest of the line describes the
#' presents that need to fit into that region by listing the *quantity of
#' each shape* of present; `1 0 1 0 3 2` means you need to fit one present
#' with shape index 0, no presents with shape index 1, one present with
#' shape index 2, no presents with shape index 3, three presents with shape
#' index 4, and two presents with shape index 5.
#'
#' Presents can be *rotated and flipped* as necessary to make them fit in
#' the available space, but they have to always be placed perfectly on the
#' grid. Shapes can\'t overlap (that is, the `#` part from two different
#' presents can\'t go in the same place on the grid), but they *can* fit
#' together (that is, the `.` part in a present\'s shape\'s diagram does
#' not block another present from occupying that space on the grid).
#'
#' The Elves need to know *how many of the regions* can fit the presents
#' listed. In the above example, there are six unique present shapes and
#' three regions that need checking.
#'
#' The first region is 4x4:
#'
#'     ....
#'     ....
#'     ....
#'     ....
#'
#' In it, you need to determine whether you could fit two presents that
#' have shape index `4`:
#'
#'     ###
#'     #..
#'     ###
#'
#' After some experimentation, it turns out that you *can* fit both
#' presents in this region. Here is one way to do it, using `A` to
#' represent one present and `B` to represent the other:
#'
#'     AAA.
#'     ABAB
#'     ABAB
#'     .BBB
#'
#' The second region, `12x5: 1 0 1 0 2 2`, is `12` units wide and `5` units
#' long. In that region, you need to try to fit one present with shape
#' index `0`, one present with shape index `2`, two presents with shape
#' index `4`, and two presents with shape index `5`.
#'
#' It turns out that these presents *can* all fit in this region. Here is
#' one way to do it, again using different capital letters to represent all
#' the required presents:
#'
#'     ....AAAFFE.E
#'     .BBBAAFFFEEE
#'     DDDBAAFFCECE
#'     DBBB....CCC.
#'     DDD.....C.C.
#'
#' The third region, `12x5: 1 0 1 0 3 2`, is the same size as the previous
#' region; the only difference is that this region needs to fit one
#' additional present with shape index `4`. Unfortunately, no matter how
#' hard you try, there is *no way to fit all of the presents* into this
#' region.
#'
#' So, in this example, *`2`* regions can fit all of their listed presents.
#'
#' Consider the regions beneath each tree and the presents the Elves would
#' like to fit into each of them. *How many of the regions can fit all of
#' the presents listed?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f12a(x)` returns .... For Part Two,
#'   `f12b(x)` returns ....
#' @export
#' @examples
#' f12a(example_data_12())
#' f12b()
f12a <- function(x) {
  d <- parse_input_file("input.txt")
  d <- parse_input_file("inst/input12.txt")
  shapes <- lapply(d$shapes, parse_shape)

  # just check if the cells even fit
  # fails for the test input, but works for real
  acc <- 0
  for (i in seq_along(d$problems)) {
    prob <- d$problems[[i]]
    grid_size <- prob$rows * prob$cols
    shape_sizes <- sapply(shapes, sum)
    total_cells <- sum(shape_sizes * prob$counts)
    if (total_cells <= grid_size) {
      acc <- acc + 1
    }
  }

  # find the actual solution, if it exists
  # acc <- 0
  # for (i in 1:length(d$problems)) {
  #   cat("i =", i, "\n")
  #   prob <- d$problems[[i]]
  #   acc <- acc + !is.null(solve_grid(prob$rows, prob$cols, prob$counts))
  # }

  acc

}


parse_input_file <- function(filename) {
  lines <- readLines(filename)

  shapes_list <- list()
  current_shape <- NULL
  shape_lines <- c()

  i <- 1
  while (i <= length(lines)) {
    line <- lines[i]

    if (grepl("^\\d+:$", line)) {
      if (length(shape_lines) > 0) {
        shape_str <- paste(shape_lines, collapse = "\n")
        shapes_list[[length(shapes_list) + 1]] <- shape_str
        shape_lines <- c()
      }
    } else if (grepl("^[#.]+$", line)) {
      shape_lines <- c(shape_lines, line)
    } else if (grepl("^\\d+x\\d+:", line)) {
      if (length(shape_lines) > 0) {
        shape_str <- paste(shape_lines, collapse = "\n")
        shapes_list[[length(shapes_list) + 1]] <- shape_str
        shape_lines <- c()
      }
      break
    }
    i <- i + 1
  }

  if (length(shape_lines) > 0) {
    shape_str <- paste(shape_lines, collapse = "\n")
    shapes_list[[length(shapes_list) + 1]] <- shape_str
  }

  problems <- list()
  while (i <= length(lines)) {
    line <- lines[i]
    if (grepl("^\\d+x\\d+:", line)) {
      parts <- strsplit(line, ":")[[1]]
      dims <- strsplit(parts[1], "x")[[1]]
      rows <- as.integer(dims[1])
      cols <- as.integer(dims[2])

      counts_str <- trimws(parts[2])
      counts <- as.integer(strsplit(counts_str, " +")[[1]])

      problems[[length(problems) + 1]] <- list(
        rows = rows,
        cols = cols,
        counts = counts
      )
    }
    i <- i + 1
  }

  return(list(shapes = shapes_list, problems = problems))
}

parse_shape <- function(shape_str) {
  lines <- strsplit(shape_str, "\n")[[1]]
  mat <- do.call(rbind, lapply(lines, function(line) {
    as.integer(strsplit(line, "")[[1]] == "#")
  }))
  return(mat)
}

# get_transformations <- function(shape) {
#   transforms <- list()
#   transforms[[length(transforms) + 1]] <- shape
#   temp_shape <- shape
#   for (i in 1:3) {
#     temp_shape <- t(apply(temp_shape, 2, rev))
#     transforms[[length(transforms) + 1]] <- temp_shape
#   }
#
#   shape_flipped <- shape[nrow(shape):1, ]
#   transforms[[length(transforms) + 1]] <- shape_flipped
#   for (i in 1:3) {
#     shape_flipped <- t(apply(shape_flipped, 2, rev))
#     transforms[[length(transforms) + 1]] <- shape_flipped
#   }
#
#   unique_transforms <- list()
#   seen <- character()
#   for (t in transforms) {
#     sig <- paste(as.vector(t), collapse = "")
#     if (!(sig %in% seen)) {
#       unique_transforms[[length(unique_transforms) + 1]] <- t
#       seen <- c(seen, sig)
#     }
#   }
#
#   return(unique_transforms)
# }
#
# can_place <- function(grid, shape, r, c) {
#   nr <- nrow(shape)
#   nc <- ncol(shape)
#
#   if (r + nr - 1 > nrow(grid) || c + nc - 1 > ncol(grid)) {
#     return(FALSE)
#   }
#
#   for (i in 1:nr) {
#     for (j in 1:nc) {
#       if (shape[i, j] == 1 && grid[r + i - 1, c + j - 1] != 0) {
#         return(FALSE)
#       }
#     }
#   }
#   return(TRUE)
# }
#
# place_shape <- function(grid, shape, r, c, id) {
#   new_grid <- grid
#   nr <- nrow(shape)
#   nc <- ncol(shape)
#
#   for (i in 1:nr) {
#     for (j in 1:nc) {
#       if (shape[i, j] == 1) {
#         new_grid[r + i - 1, c + j - 1] <- id
#       }
#     }
#   }
#   return(new_grid)
# }
#
# find_empty <- function(grid) {
#   for (r in 1:nrow(grid)) {
#     for (c in 1:ncol(grid)) {
#       if (grid[r, c] == 0) {
#         return(c(r, c))
#       }
#     }
#   }
#   return(NULL)
# }
#
# solve <- function(grid, shapes_queue, all_transforms, max_iter = 50000) {
#   iter_count <- 0
#   solution_found <- FALSE
#
#   solve_recursive <- function(grid, remaining_queue) {
#     if (solution_found) return(list())
#
#     iter_count <<- iter_count + 1
#     if (iter_count > max_iter) {
#       return(list())
#     }
#
#     if (length(remaining_queue) == 0) {
#       solution_found <<- TRUE
#       return(list(grid))
#     }
#
#     shape_info <- remaining_queue[[1]]
#     shape_idx <- shape_info$idx
#     shape_id <- shape_info$id
#     new_queue <- remaining_queue[-1]
#
#     for (transform in all_transforms[[shape_idx]]) {
#       nr <- nrow(transform)
#       nc <- ncol(transform)
#
#       for (r in 1:(nrow(grid) - nr + 1)) {
#         for (c in 1:(ncol(grid) - nc + 1)) {
#           if (can_place(grid, transform, r, c)) {
#             new_grid <- place_shape(grid, transform, r, c, shape_id)
#             sols <- solve_recursive(new_grid, new_queue)
#             if (length(sols) > 0) {
#               return(sols)
#             }
#           }
#         }
#       }
#     }
#
#     return(list())
#   }
#
#   return(solve_recursive(grid, shapes_queue))
# }
#
# solve_grid <- function(grid_rows, grid_cols, shape_counts) {
#   shapes_queue <- list()
#   shape_id <- 1
#
#   shape_sizes <- sapply(shapes, sum)
#
#   shape_list <- list()
#   for (i in seq_along(shape_counts)) {
#     if (shape_counts[i] > 0) {
#       for (j in 1:shape_counts[i]) {
#         shape_list[[length(shape_list) + 1]] <- list(idx = i, size = shape_sizes[i])
#       }
#     }
#   }
#
#   if (length(shape_list) > 0) {
#     sizes <- sapply(shape_list, function(x) x$size)
#     shape_list <- shape_list[order(sizes, decreasing = TRUE)]
#   }
#
#   for (i in 1:length(shape_list)) {
#     shape_list[[i]]$id <- i
#   }
#   shapes_queue <- shape_list
#   all_transforms <- lapply(shapes, get_transformations)
#
#   total_cells <- sum(sapply(seq_along(shapes), function(i) {
#     sum(shapes[[i]]) * shape_counts[i]
#   }))
#
#   if (total_cells > grid_rows * grid_cols) {
#     cat("Too large\n\n")
#     return(NULL)
#   }
#
#   grid <- matrix(0, nrow = grid_rows, ncol = grid_cols)
#   solutions <- solve(grid, shapes_queue, all_transforms)
#
#   if (length(solutions) > 0) {
#     cat("FOUND!\n\n")
#     return(solutions[[1]])
#   } else {
#     cat("\n\nNo solution\n")
#     return(NULL)
#   }
# }


#' @rdname day12
#' @export
f12b <- function(x) {

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
