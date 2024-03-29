% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/day05.R
\name{day05}
\alias{day05}
\alias{f05a}
\alias{f05b}
\alias{example_data_05}
\title{Day 05: If You Give A Seed A Fertilizer}
\usage{
f05a(x)

f05b(x)

example_data_05(example = 1)
}
\arguments{
\item{x}{some data}

\item{example}{Which example data to use (by position or name). Defaults to
1.}
}
\value{
For Part One, \code{f05a(x)} returns .... For Part Two,
\code{f05b(x)} returns ....
}
\description{
\href{https://adventofcode.com/2023/day/5}{If You Give A Seed A Fertilizer}
}
\details{
\strong{Part One}

You take the boat and find the gardener right where you were told he
would be: managing a giant \"garden\" that looks more to you like a
farm.

\"A water source? Island Island \emph{is} the water source!\" You point out
that Snow Island isn\'t receiving any water.

\"Oh, we had to stop the water because we \emph{ran out of sand} to
\href{https://en.wikipedia.org/wiki/Sand_filter}{filter}{target="_blank"} it
with! Can\'t make snow with dirty water. Don\'t worry, I\'m sure we\'ll
get more sand soon; we only turned off the water a few days\...
weeks\... oh no.\" His face sinks into a look of horrified realization.

\"I\'ve been so busy making sure everyone here has food that I
completely forgot to check why we stopped getting more sand! There\'s a
ferry leaving soon that is headed over in that direction - it\'s much
faster than your boat. Could you please go check it out?\"

You barely have time to agree to this request when he brings up another.
\"While you wait for the ferry, maybe you can help us with our \emph{food
production problem}. The latest Island Island
\href{https://en.wikipedia.org/wiki/Almanac}{Almanac}{target="_blank"} just
arrived and we\'re having trouble making sense of it.\"

The almanac (your puzzle input) lists all of the seeds that need to be
planted. It also lists what type of soil to use with each kind of seed,
what type of fertilizer to use with each kind of soil, what type of
water to use with each kind of fertilizer, and so on. Every type of
seed, soil, fertilizer and so on is identified with a number, but
numbers are reused by each category - that is, soil \code{123} and fertilizer
\code{123} aren\'t necessarily related to each other.

For example:

\if{html}{\out{<div class="sourceCode">}}\preformatted{seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4
}\if{html}{\out{</div>}}

The almanac starts by listing which seeds need to be planted: seeds
\code{79}, \code{14}, \code{55}, and \code{13}.

The rest of the almanac contains a list of \emph{maps} which describe how to
convert numbers from a \emph{source category} into numbers in a \emph{destination
category}. That is, the section that starts with \verb{seed-to-soil map:}
describes how to convert a \emph{seed number} (the source) to a \emph{soil number}
(the destination). This lets the gardener and his team know which soil
to use with which seeds, which water to use with which fertilizer, and
so on.

Rather than list every source number and its corresponding destination
number one by one, the maps describe entire \emph{ranges} of numbers that can
be converted. Each line within a map contains [three
numbers]{title="Don't blame me for the weird order. Blame LXC container.conf UID mappings."}:
the \emph{destination range start}, the \emph{source range start}, and the \emph{range
length}.

Consider again the example \verb{seed-to-soil map}:

\if{html}{\out{<div class="sourceCode">}}\preformatted{50 98 2
52 50 48
}\if{html}{\out{</div>}}

The first line has a \emph{destination range start} of \code{50}, a \emph{source range
start} of \code{98}, and a \emph{range length} of \code{2}. This line means that the
source range starts at \code{98} and contains two values: \code{98} and \code{99}. The
destination range is the same length, but it starts at \code{50}, so its two
values are \code{50} and \code{51}. With this information, you know that seed
number \code{98} corresponds to soil number \code{50} and that seed number \code{99}
corresponds to soil number \code{51}.

The second line means that the source range starts at \code{50} and contains
\code{48} values: \code{50}, \code{51}, \..., \code{96}, \code{97}. This corresponds to a
destination range starting at \code{52} and also containing \code{48} values:
\code{52}, \code{53}, \..., \code{98}, \code{99}. So, seed number \code{53} corresponds to soil
number \code{55}.

Any source numbers that \emph{aren\'t mapped} correspond to the \emph{same}
destination number. So, seed number \code{10} corresponds to soil number
\code{10}.

So, the entire list of seed numbers and their corresponding soil numbers
looks like this:

\if{html}{\out{<div class="sourceCode">}}\preformatted{seed  soil
0     0
1     1
...   ...
48    48
49    49
50    52
51    53
...   ...
96    98
97    99
98    50
99    51
}\if{html}{\out{</div>}}

With this map, you can look up the soil number required for each initial
seed number:
\itemize{
\item Seed number \code{79} corresponds to soil number \code{81}.
\item Seed number \code{14} corresponds to soil number \code{14}.
\item Seed number \code{55} corresponds to soil number \code{57}.
\item Seed number \code{13} corresponds to soil number \code{13}.
}

The gardener and his team want to get started as soon as possible, so
they\'d like to know the closest location that needs a seed. Using these
maps, find \emph{the lowest location number that corresponds to any of the
initial seeds}. To do this, you\'ll need to convert each seed number
through other categories until you can find its corresponding \emph{location
number}. In this example, the corresponding types are:
\itemize{
\item Seed \code{79}, soil \code{81}, fertilizer \code{81}, water \code{81}, light \code{74},
temperature \code{78}, humidity \code{78}, \emph{location \code{82}}.
\item Seed \code{14}, soil \code{14}, fertilizer \code{53}, water \code{49}, light \code{42},
temperature \code{42}, humidity \code{43}, \emph{location \code{43}}.
\item Seed \code{55}, soil \code{57}, fertilizer \code{57}, water \code{53}, light \code{46},
temperature \code{82}, humidity \code{82}, \emph{location \code{86}}.
\item Seed \code{13}, soil \code{13}, fertilizer \code{52}, water \code{41}, light \code{34},
temperature \code{34}, humidity \code{35}, \emph{location \code{35}}.
}

So, the lowest location number in this example is \emph{\code{35}}.

\emph{What is the lowest location number that corresponds to any of the
initial seed numbers?}

\strong{Part Two}

Everyone will starve if you only plant such a small number of seeds.
Re-reading the almanac, it looks like the \verb{seeds:} line actually
describes \emph{ranges of seed numbers}.

The values on the initial \verb{seeds:} line come in pairs. Within each pair,
the first value is the \emph{start} of the range and the second value is the
\emph{length} of the range. So, in the first line of the example above:

\if{html}{\out{<div class="sourceCode">}}\preformatted{seeds: 79 14 55 13
}\if{html}{\out{</div>}}

This line describes two ranges of seed numbers to be planted in the
garden. The first range starts with seed number \code{79} and contains \code{14}
values: \code{79}, \code{80}, \..., \code{91}, \code{92}. The second range starts with seed
number \code{55} and contains \code{13} values: \code{55}, \code{56}, \..., \code{66}, \code{67}.

Now, rather than considering four seed numbers, you need to consider a
total of \emph{27} seed numbers.

In the above example, the lowest location number can be obtained from
seed number \code{82}, which corresponds to soil \code{84}, fertilizer \code{84}, water
\code{84}, light \code{77}, temperature \code{45}, humidity \code{46}, and \emph{location \code{46}}.
So, the lowest location number is \emph{\code{46}}.

Consider all of the initial seed numbers listed in the ranges on the
first line of the almanac. \emph{What is the lowest location number that
corresponds to any of the initial seed numbers?}
}
\examples{
f05a(example_data_05())
f05b()
}
