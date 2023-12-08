#' Day 08: Haunted Wasteland
#'
#' [Haunted Wasteland](https://adventofcode.com/2023/day/8)
#'
#' @name day08
#' @rdname day08
#' @details
#'
#' **Part One**
#'
#' You\'re still riding a camel across Desert Island when you spot a
#' sandstorm quickly approaching. When you turn to warn the Elf, she
#' disappears before your eyes! To be fair, she had just finished warning
#' you about *ghosts* a few minutes ago.
#' 
#' One of the camel\'s pouches is labeled \"maps\" - sure enough, it\'s
#' full of documents (your puzzle input) about how to navigate the desert.
#' At least, you\'re pretty sure that\'s what they are; one of the
#' documents contains a list of left/right instructions, and the rest of
#' the documents seem to describe some kind of *network* of labeled nodes.
#' 
#' It seems like you\'re meant to use the *left/right* instructions to
#' *navigate the network*. Perhaps if you have the camel follow the same
#' instructions, you can escape the haunted wasteland!
#' 
#' After examining the maps for a bit, two nodes stick out: `AAA` and
#' `ZZZ`. You feel like `AAA` is where you are now, and you have to follow
#' the left/right instructions until you reach `ZZZ`.
#' 
#' This format defines each *node* of the network individually. For
#' example:
#' 
#'     RL
#' 
#'     AAA = (BBB, CCC)
#'     BBB = (DDD, EEE)
#'     CCC = (ZZZ, GGG)
#'     DDD = (DDD, DDD)
#'     EEE = (EEE, EEE)
#'     GGG = (GGG, GGG)
#'     ZZZ = (ZZZ, ZZZ)
#' 
#' Starting with `AAA`, you need to *look up the next element* based on the
#' next left/right instruction in your input. In this example, start with
#' `AAA` and go *right* (`R`) by choosing the right element of `AAA`,
#' *`CCC`*. Then, `L` means to choose the *left* element of `CCC`, *`ZZZ`*.
#' By following the left/right instructions, you reach `ZZZ` in *`2`*
#' steps.
#' 
#' Of course, you might not find `ZZZ` right away. If you run out of
#' left/right instructions, repeat the whole sequence of instructions as
#' necessary: `RL` really means `RLRLRLRLRLRLRLRL...` and so on. For
#' example, here is a situation that takes *`6`* steps to reach `ZZZ`:
#' 
#'     LLR
#' 
#'     AAA = (BBB, BBB)
#'     BBB = (AAA, ZZZ)
#'     ZZZ = (ZZZ, ZZZ)
#' 
#' Starting at `AAA`, follow the left/right instructions. *How many steps
#' are required to reach `ZZZ`?*
#'
#' **Part Two**
#' 
#' The [sandstorm]{title="Duhduhduhduhduh! Dah, duhduhduhduhduh!"} is upon
#' you and you aren\'t any closer to escaping the wasteland. You had the
#' camel follow the instructions, but you\'ve barely left your starting
#' position. It\'s going to take *significantly more steps* to escape!
#' 
#' What if the map isn\'t for people - what if the map is for *ghosts*? Are
#' ghosts even bound by the laws of spacetime? Only one way to find out.
#' 
#' After examining the maps a bit longer, your attention is drawn to a
#' curious fact: the number of nodes with names ending in `A` is equal to
#' the number ending in `Z`! If you were a ghost, you\'d probably just
#' *start at every node that ends with `A`* and follow all of the paths at
#' the same time until they all simultaneously end up at nodes that end
#' with `Z`.
#' 
#' For example:
#' 
#'     LR
#' 
#'     11A = (11B, XXX)
#'     11B = (XXX, 11Z)
#'     11Z = (11B, XXX)
#'     22A = (22B, XXX)
#'     22B = (22C, 22C)
#'     22C = (22Z, 22Z)
#'     22Z = (22B, 22B)
#'     XXX = (XXX, XXX)
#' 
#' Here, there are two starting nodes, `11A` and `22A` (because they both
#' end with `A`). As you follow each left/right instruction, use that
#' instruction to *simultaneously* navigate away from both nodes you\'re
#' currently on. Repeat this process until *all* of the nodes you\'re
#' currently on end with `Z`. (If only some of the nodes you\'re on end
#' with `Z`, they act like any other node and you continue as normal.) In
#' this example, you would proceed as follows:
#' 
#' -   Step 0: You are at `11A` and `22A`.
#' -   Step 1: You choose all of the *left* paths, leading you to `11B` and
#'     `22B`.
#' -   Step 2: You choose all of the *right* paths, leading you to *`11Z`*
#'     and `22C`.
#' -   Step 3: You choose all of the *left* paths, leading you to `11B` and
#'     *`22Z`*.
#' -   Step 4: You choose all of the *right* paths, leading you to *`11Z`*
#'     and `22B`.
#' -   Step 5: You choose all of the *left* paths, leading you to `11B` and
#'     `22C`.
#' -   Step 6: You choose all of the *right* paths, leading you to *`11Z`*
#'     and *`22Z`*.
#' 
#' So, in this example, you end up entirely on nodes that end in `Z` after
#' *`6`* steps.
#' 
#' Simultaneously start on every node that ends with `A`. *How many steps
#' does it take before you\'re only on nodes that end with `Z`?*
#'
#' @param x some data
#' @return For Part One, `f08a(x)` returns .... For Part Two,
#'   `f08b(x)` returns ....
#' @export
#' @examples
#' f08a(example_data_08())
#' f08b()
f08a <- function(x) {
  inst <- strrep(x[1], 100)
  nodes <- parse_nodes(x[3:length(x)])
  this_node <- "AAA"
  steps <- 0
  for (i in strsplit(inst, "")[[1]]) {
    dir <- ifelse(i == "L", 1, 2)
    this_node <- nodes[[this_node]][dir]
    steps <- steps + 1
    if (this_node == "ZZZ")
      break
  }
  if (this_node != "ZZZ") stop("not enough")
  steps
}

#' @rdname day08
#' @export
f08b <- function(x) {
  inst <- strrep(x[1], 100000)
  nodes <- parse_nodes(x[3:length(x)])
  this_node <- grep("A$", names(nodes), value = TRUE)
  node_steps <- c()
  for (n in this_node) {
    node_i <- n
    steps <- 0
    for (i in strsplit(inst, "")[[1]]) {
      dir <- ifelse(i == "L", 1, 2)
      node_i <- nodes[[node_i]][dir]
      steps <- steps + 1
      if (grepl("Z$", node_i))
        break
    }
    if (!grepl("Z$", node_i)) stop("not enough")
    node_steps[n] <- steps
  }
  # print(node_steps)
  sprintf("%.15g", Reduce(pracma::Lcm, node_steps))
}


parse_nodes <- function(x) {
  xx <- strsplit(x, " = ")
  names <- sapply(xx, \(y) y[1])
  dirs <- strsplit(sapply(xx, \(y) y[2]), ", ")
  dirs <- lapply(dirs, \(y) gsub("[)()]", "", y))
  setNames(dirs, names)
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
