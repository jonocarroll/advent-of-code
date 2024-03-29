% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/day14.R
\name{day14}
\alias{day14}
\alias{f14a}
\alias{f14b}
\alias{example_data_14}
\title{Day 14: Parabolic Reflector Dish}
\usage{
f14a(x)

f14b(x)

example_data_14(example = 1)
}
\arguments{
\item{x}{some data}

\item{example}{Which example data to use (by position or name). Defaults to
1.}
}
\value{
For Part One, \code{f14a(x)} returns .... For Part Two,
\code{f14b(x)} returns ....
}
\description{
\href{https://adventofcode.com/2023/day/14}{Parabolic Reflector Dish}
}
\details{
\strong{Part One}

You reach the place where all of the mirrors were pointing: a massive
\href{https://en.wikipedia.org/wiki/Parabolic_reflector}{parabolic reflector dish}{target="_blank"}
[attached]{title="Why, where do you attach YOUR massive parabolic reflector dishes?"}
to the side of another large mountain.

The dish is made up of many small mirrors, but while the mirrors
themselves are roughly in the shape of a parabolic reflector dish, each
individual mirror seems to be pointing in slightly the wrong direction.
If the dish is meant to focus light, all it\'s doing right now is
sending it in a vague direction.

This system must be what provides the energy for the lava! If you focus
the reflector dish, maybe you can go where it\'s pointing and use the
light to fix the lava production.

Upon closer inspection, the individual mirrors each appear to be
connected via an elaborate system of ropes and pulleys to a large metal
platform below the dish. The platform is covered in large rocks of
various shapes. Depending on their position, the weight of the rocks
deforms the platform, and the shape of the platform controls which ropes
move and ultimately the focus of the dish.

In short: if you move the rocks, you can focus the dish. The platform
even has a control panel on the side that lets you \emph{tilt} it in one of
four directions! The rounded rocks (\code{O}) will roll when the platform is
tilted, while the cube-shaped rocks (\verb{#}) will stay in place. You note
the positions of all of the empty spaces (\code{.}) and rocks (your puzzle
input). For example:

\if{html}{\out{<div class="sourceCode">}}\preformatted{O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#....
}\if{html}{\out{</div>}}

Start by tilting the lever so all of the rocks will slide \emph{north} as far
as they will go:

\if{html}{\out{<div class="sourceCode">}}\preformatted{OOOO.#.O..
OO..#....#
OO..O##..O
O..#.OO...
........#.
..#....#.#
..O..#.O.O
..O.......
#....###..
#....#....
}\if{html}{\out{</div>}}

You notice that the support beams along the north side of the platform
are \emph{damaged}; to ensure the platform doesn\'t collapse, you should
calculate the \emph{total load} on the north support beams.

The amount of load caused by a single rounded rock (\code{O}) is equal to the
number of rows from the rock to the south edge of the platform,
including the row the rock is on. (Cube-shaped rocks (\verb{#}) don\'t
contribute to load.) So, the amount of load caused by each rock in each
row is as follows:

\if{html}{\out{<div class="sourceCode">}}\preformatted{OOOO.#.O.. 10
OO..#....#  9
OO..O##..O  8
O..#.OO...  7
........#.  6
..#....#.#  5
..O..#.O.O  4
..O.......  3
#....###..  2
#....#....  1
}\if{html}{\out{</div>}}

The total load is the sum of the load caused by all of the \emph{rounded
rocks}. In this example, the total load is \emph{\code{136}}.

Tilt the platform so that the rounded rocks all roll north. Afterward,
\emph{what is the total load on the north support beams?}

\strong{Part Two}

The parabolic reflector dish deforms, but not in a way that focuses the
beam. To do that, you\'ll need to move the rocks to the edges of the
platform. Fortunately, a button on the side of the control panel labeled
\"\emph{spin cycle}\" attempts to do just that!

Each \emph{cycle} tilts the platform four times so that the rounded rocks
roll \emph{north}, then \emph{west}, then \emph{south}, then \emph{east}. After each tilt,
the rounded rocks roll as far as they can before the platform tilts in
the next direction. After one cycle, the platform will have finished
rolling the rounded rocks in those four directions in that order.

Here\'s what happens in the example above after each of the first few
cycles:

\if{html}{\out{<div class="sourceCode">}}\preformatted{After 1 cycle:
.....#....
....#...O#
...OO##...
.OO#......
.....OOO#.
.O#...O#.#
....O#....
......OOOO
#...O###..
#..OO#....

After 2 cycles:
.....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#..OO###..
#.OOO#...O

After 3 cycles:
.....#....
....#...O#
.....##...
..O#......
.....OOO#.
.O#...O#.#
....O#...O
.......OOO
#...O###.O
#.OOO#...O
}\if{html}{\out{</div>}}

This process should work if you leave it running long enough, but
you\'re still worried about the north support beams. To make sure
they\'ll survive for a while, you need to calculate the \emph{total load} on
the north support beams after \code{1000000000} cycles.

In the above example, after \code{1000000000} cycles, the total load on the
north support beams is \emph{\code{64}}.

Run the spin cycle for \code{1000000000} cycles. Afterward, \emph{what is the
total load on the north support beams?}
}
\examples{
f14a(example_data_14())
f14b()
}
