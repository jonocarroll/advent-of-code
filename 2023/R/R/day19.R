#' Day 19: Aplenty
#'
#' [Aplenty](https://adventofcode.com/2023/day/19)
#'
#' @name day19
#' @rdname day19
#' @details
#'
#' **Part One**
#'
#' The Elves of Gear Island are thankful for your help and send you on your
#' way. They even have a hang glider that someone [stole](9) from Desert
#' Island; since you\'re already going that direction, it would help them a
#' lot if you would use it to get down there and return it to them.
#' 
#' As you reach the bottom of the *relentless avalanche of machine parts*,
#' you discover that they\'re already forming a formidable heap. Don\'t
#' worry, though - a group of Elves is already here organizing the parts,
#' and they have a
#' [*system*]{title="This part sparks joy. This part sparks joy. This part ALSO sparks joy... I think we need a different system."}.
#' 
#' To start, each part is rated in each of four categories:
#' 
#' -   `x`: E*x*tremely cool looking
#' -   `m`: *M*usical (it makes a noise when you hit it)
#' -   `a`: *A*erodynamic
#' -   `s`: *S*hiny
#' 
#' Then, each part is sent through a series of *workflows* that will
#' ultimately *accept* or *reject* the part. Each workflow has a name and
#' contains a list of *rules*; each rule specifies a condition and where to
#' send the part if the condition is true. The first rule that matches the
#' part being considered is applied immediately, and the part moves on to
#' the destination described by the rule. (The last rule in each workflow
#' has no condition and always applies if reached.)
#' 
#' Consider the workflow `ex{x>10:one,m<20:two,a>30:R,A}`. This workflow is
#' named `ex` and contains four rules. If workflow `ex` were considering a
#' specific part, it would perform the following steps in order:
#' 
#' -   Rule \"`x>10:one`\": If the part\'s `x` is more than `10`, send the
#'     part to the workflow named `one`.
#' -   Rule \"`m<20:two`\": Otherwise, if the part\'s `m` is less than
#'     `20`, send the part to the workflow named `two`.
#' -   Rule \"`a>30:R`\": Otherwise, if the part\'s `a` is more than `30`,
#'     the part is immediately *rejected* (`R`).
#' -   Rule \"`A`\": Otherwise, because no other rules matched the part,
#'     the part is immediately *accepted* (`A`).
#' 
#' If a part is sent to another workflow, it immediately switches to the
#' start of that workflow instead and never returns. If a part is
#' *accepted* (sent to `A`) or *rejected* (sent to `R`), the part
#' immediately stops any further processing.
#' 
#' The system works, but it\'s not keeping up with the torrent of weird
#' metal shapes. The Elves ask if you can help sort a few parts and give
#' you the list of workflows and some part ratings (your puzzle input). For
#' example:
#' 
#'     px{a2090:A,rfg}
#'     pv{a>1716:R,A}
#'     lnx{m>1548:A,A}
#'     rfg{s2440:R,A}
#'     qs{s>3448:A,lnx}
#'     qkq{x2662:A,R}
#'     in{s2770:qs,m3333:R,R}
#'     hdj{m>838:A,pv}
#' 
#'     {x=787,m=2655,a=1222,s=2876}
#'     {x=1679,m=44,a=2067,s=496}
#'     {x=2036,m=264,a=79,s=2244}
#'     {x=2461,m=1339,a=466,s=291}
#'     {x=2127,m=1623,a=2188,s=1013}
#' 
#' The workflows are listed first, followed by a blank line, then the
#' ratings of the parts the Elves would like you to sort. All parts begin
#' in the workflow named `in`. In this example, the five listed parts go
#' through the following workflows:
#' 
#' -   `{x=787,m=2655,a=1222,s=2876}`: `in` -\> `qqz` -\> `qs` -\> `lnx`
#'     -\> *`A`*
#' -   `{x=1679,m=44,a=2067,s=496}`: `in` -\> `px` -\> `rfg` -\> `gd` -\>
#'     *`R`*
#' -   `{x=2036,m=264,a=79,s=2244}`: `in` -\> `qqz` -\> `hdj` -\> `pv` -\>
#'     *`A`*
#' -   `{x=2461,m=1339,a=466,s=291}`: `in` -\> `px` -\> `qkq` -\> `crn` -\>
#'     *`R`*
#' -   `{x=2127,m=1623,a=2188,s=1013}`: `in` -\> `px` -\> `rfg` -\> *`A`*
#' 
#' Ultimately, three parts are *accepted*. Adding up the `x`, `m`, `a`, and
#' `s` rating for each of the accepted parts gives `7540` for the part with
#' `x=787`, `4623` for the part with `x=2036`, and `6951` for the part with
#' `x=2127`. Adding all of the ratings for *all* of the accepted parts
#' gives the sum total of *`19114`*.
#' 
#' Sort through all of the parts you\'ve been given; *what do you get if
#' you add together all of the rating numbers for all of the parts that
#' ultimately get accepted?*
#'
#' **Part Two**
#' 
#' Even with your help, the sorting process *still* isn\'t fast enough.
#' 
#' One of the Elves comes up with a new plan: rather than sort parts
#' individually through all of these workflows, maybe you can figure out in
#' advance which combinations of ratings will be accepted or rejected.
#' 
#' Each of the four ratings (`x`, `m`, `a`, `s`) can have an integer value
#' ranging from a minimum of `1` to a maximum of `4000`. Of *all possible
#' distinct combinations* of ratings, your job is to figure out which ones
#' will be *accepted*.
#' 
#' In the above example, there are *`167409079868000`* distinct
#' combinations of ratings that will be accepted.
#' 
#' Consider only your list of workflows; the list of part ratings that the
#' Elves wanted you to sort is no longer relevant. *How many distinct
#' combinations of ratings will be accepted by the Elves\' workflows?*
#'
#' @param x some data
#' @return For Part One, `f19a(x)` returns .... For Part Two,
#'   `f19b(x)` returns ....
#' @export
#' @examples
#' f19a(example_data_19())
#' f19b()
f19a <- function(x) {
  splitat <- which(x == "")
  inst <- x[1:(splitat-1)]
  parts <- x[(splitat+1):length(x)]
  for (i in seq_along(inst)) {
    eval(str2lang(translate_inst(inst[i])))
  }
  a <- 0
  for (p in seq_along(parts)) {
    a <- a + do.call(f_in, as.list(translate_parts(parts[p])))
  }
  a
}

A <-function(x, m, a, s) {
  sum(c(x, m, a, s))
}

R <- function(x, m, a, s) {
  0
}

translate_inst <- function(s) {
  name <- sub("\\{.*$", "", s)
  if (name == "in") name <- "f_in"
  rest <- sub("\\}", "", sub("^.*\\{", "", s))
  conds <- strsplit(rest, ",")[[1]]
  conds <- strsplit(conds, ":")
  glue::glue("|name| <- function(x, m, a, s) {
    |paste(sapply(conds, makefun), collapse = '\\n')|
  }", .open = "|", .close = "|")
}

makefun <- function(w) {
  if (length(w) > 1) {
    glue::glue("if ({w[1]}) return({w[2]}(x,m,a,s))")
  } else {
    glue::glue("{w[1]}(x,m,a,s)")
  }
}

get_conds <- function(s) {
  name <- sub("\\{.*$", "", s)
  if (name == "in") name <- "f_in"
  sub("\\}", "", sub("^.*\\{", "", s))
}

split_range <- function(all_instr, 
                        instr, 
                        pts = list(x = 1:4000, m = 1:4000, a = 1:4000, s = 1:4000)) {
  if (instr == "A") return(pts)
  if (instr == "R") return(NULL)
  instr <- all_instr[[instr]]
  acc <- vector("list")
  rest <- sub("\\}", "", sub("^.*\\{", "", instr))
  conds <- strsplit(rest, ",")[[1]]
  res <- strsplit(conds, ":")
  for (i in res) {
    if (i[1] == "R") return(acc)
    if (i[1] == "A") return(c(acc, list(pts)))
    if (length(i) == 1) {
      acc <- c(acc, split_range(all_instr, i[1], pts))
      return(acc)
    }
    pts_A <- pts
    xmas <- substr(i[1], 1, 1)
    pts_A[[xmas]] <- pts[[xmas]][eval(parse(text = i[1]), envir = pts)]
    pts[[xmas]] <- pts[[xmas]][!eval(parse(text = i[1]), envir = pts)]
    if (i[2] == "A") {acc <- c(acc, list(pts_A)); next}
    if (i[2] == "R") next
    acc <- c(acc, split_range(all_instr, i[2], pts_A))
  }
  acc
}


translate_parts <- function(s) {
  as.integer(strsplit(gsub("[^0-9,]", "", s), ",")[[1]])
}

#' @rdname day19
#' @export
f19b <- function(x) {
  splitat <- which(x == "")
  inst <- x[1:(splitat-1)]
  conds <- sapply(inst, get_conds)
  condnames <- sub("\\{.*", "", names(conds))
  conds <- setNames(conds, condnames)
  sprintf("%.15g", sum(sapply(split_range(conds, "in"), \(x) prod(lengths(x)))))
}


f19_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day19
#' @export
example_data_19 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
