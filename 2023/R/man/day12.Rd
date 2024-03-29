% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/day12.R
\name{day12}
\alias{day12}
\alias{f12a}
\alias{f12b}
\alias{example_data_12}
\title{Day 12: Hot Springs}
\usage{
f12a(x)

f12b(x)

example_data_12(example = 1)
}
\arguments{
\item{x}{some data}

\item{example}{Which example data to use (by position or name). Defaults to
1.}
}
\value{
For Part One, \code{f12a(x)} returns .... For Part Two,
\code{f12b(x)} returns ....
}
\description{
\href{https://adventofcode.com/2023/day/12}{Hot Springs}
}
\details{
\strong{Part One}

You finally reach the hot springs! You can see steam rising from
secluded areas attached to the primary, ornate building.

As you turn to enter, the \href{11}{researcher} stops you. \"Wait - I thought
you were looking for the hot springs, weren\'t you?\" You indicate that
this definitely looks like hot springs to you.

\"Oh, sorry, common mistake! This is actually the
\href{https://en.wikipedia.org/wiki/Onsen}{onsen}{target="_blank"}! The hot
springs are next door.\"

You look in the direction the researcher is pointing and suddenly notice
the [massive metal helixes]{title="I love this joke. I'm not sorry."}
towering overhead. \"This way!\"

It only takes you a few more steps to reach the main gate of the massive
fenced-off area containing the springs. You go through the gate and into
a small administrative building.

\"Hello! What brings you to the hot springs today? Sorry they\'re not
very hot right now; we\'re having a \emph{lava shortage} at the moment.\" You
ask about the missing machine parts for Desert Island.

\"Oh, all of Gear Island is currently offline! Nothing is being
manufactured at the moment, not until we get more lava to heat our
forges. And our springs. The springs aren\'t very springy unless
they\'re hot!\"

\"Say, could you go up and see why the lava stopped flowing? The springs
are too cold for normal operation, but we should be able to find one
springy enough to launch \emph{you} up there!\"

There\'s just one problem - many of the springs have fallen into
disrepair, so they\'re not actually sure which springs would even be
\emph{safe} to use! Worse yet, their \emph{condition records of which springs are
damaged} (your puzzle input) are also damaged! You\'ll need to help them
repair the damaged records.

In the giant field just outside, the springs are arranged into \emph{rows}.
For each row, the condition records show every spring and whether it is
\emph{operational} (\code{.}) or \emph{damaged} (\verb{#}). This is the part of the
condition records that is itself damaged; for some springs, it is simply
\emph{unknown} (\verb{?}) whether the spring is operational or damaged.

However, the engineer that produced the condition records also
duplicated some of this information in a different format! After the
list of springs for a given row, the size of each \emph{contiguous group of
damaged springs} is listed in the order those groups appear in the row.
This list always accounts for every damaged spring, and each number is
the entire size of its contiguous group (that is, groups are always
separated by at least one operational spring: \verb{####} would always be
\code{4}, never \verb{2,2}).

So, condition records with no unknown spring conditions might look like
this:

\if{html}{\out{<div class="sourceCode">}}\preformatted{#.#.### 1,1,3
.#...#....###. 1,1,3
.#.###.#.###### 1,3,1,6
####.#...#... 4,1,1
#....######..#####. 1,6,5
.###.##....# 3,2,1
}\if{html}{\out{</div>}}

However, the condition records are partially damaged; some of the
springs\' conditions are actually \emph{unknown} (\verb{?}). For example:

\if{html}{\out{<div class="sourceCode">}}\preformatted{???.### 1,1,3
.??..??...?##. 1,1,3
?#?#?#?#?#?#?#? 1,3,1,6
????.#...#... 4,1,1
????.######..#####. 1,6,5
?###???????? 3,2,1
}\if{html}{\out{</div>}}

Equipped with this information, it is your job to figure out \emph{how many
different arrangements} of operational and broken springs fit the given
criteria in each row.

In the first line (\code{???.### 1,1,3}), there is exactly \emph{one} way separate
groups of one, one, and three broken springs (in that order) can appear
in that row: the first three unknown springs must be broken, then
operational, then broken (\verb{#.#}), making the whole row \verb{#.#.###}.

The second line is more interesting: \verb{.??..??...?##. 1,1,3} could be a
total of \emph{four} different arrangements. The last \verb{?} must always be
broken (to satisfy the final contiguous group of three broken springs),
and each \verb{??} must hide exactly one of the two broken springs. (Neither
\verb{??} could be both broken springs or they would form a single contiguous
group of two; if that were true, the numbers afterward would have been
\verb{2,3} instead.) Since each \verb{??} can either be \verb{#.} or \code{.#}, there are
four possible arrangements of springs.

The last line is actually consistent with \emph{ten} different arrangements!
Because the first number is \code{3}, the first and second \verb{?} must both be
\code{.} (if either were \verb{#}, the first number would have to be \code{4} or
higher). However, the remaining run of unknown spring conditions have
many different ways they could hold groups of two and one broken
springs:

\if{html}{\out{<div class="sourceCode">}}\preformatted{?###???????? 3,2,1
.###.##.#...
.###.##..#..
.###.##...#.
.###.##....#
.###..##.#..
.###..##..#.
.###..##...#
.###...##.#.
.###...##..#
.###....##.#
}\if{html}{\out{</div>}}

In this example, the number of possible arrangements for each row is:
\itemize{
\item \code{???.### 1,1,3} - \emph{\code{1}} arrangement
\item \verb{.??..??...?##. 1,1,3} - \emph{\code{4}} arrangements
\item \verb{?#?#?#?#?#?#?#? 1,3,1,6} - \emph{\code{1}} arrangement
\item \code{????.#...#... 4,1,1} - \emph{\code{1}} arrangement
\item \code{????.######..#####. 1,6,5} - \emph{\code{4}} arrangements
\item \verb{?###???????? 3,2,1} - \emph{\code{10}} arrangements
}

Adding all of the possible arrangement counts together produces a total
of \emph{\code{21}} arrangements.

For each row, count all of the different arrangements of operational and
broken springs that meet the given criteria. \emph{What is the sum of those
counts?}

\strong{Part Two}

As you look out at the field of springs, you feel like there are way
more springs than the condition records list. When you examine the
records, you discover that they were actually \emph{folded up} this whole
time!

To \emph{unfold the records}, on each row, replace the list of spring
conditions with five copies of itself (separated by \verb{?}) and replace the
list of contiguous groups of damaged springs with five copies of itself
(separated by \verb{,}).

So, this row:

\if{html}{\out{<div class="sourceCode">}}\preformatted{.# 1
}\if{html}{\out{</div>}}

Would become:

\if{html}{\out{<div class="sourceCode">}}\preformatted{.#?.#?.#?.#?.# 1,1,1,1,1
}\if{html}{\out{</div>}}

The first line of the above example would become:

\if{html}{\out{<div class="sourceCode">}}\preformatted{???.###????.###????.###????.###????.### 1,1,3,1,1,3,1,1,3,1,1,3,1,1,3
}\if{html}{\out{</div>}}

In the above example, after unfolding, the number of possible
arrangements for some rows is now much larger:
\itemize{
\item \code{???.### 1,1,3} - \emph{\code{1}} arrangement
\item \verb{.??..??...?##. 1,1,3} - \emph{\code{16384}} arrangements
\item \verb{?#?#?#?#?#?#?#? 1,3,1,6} - \emph{\code{1}} arrangement
\item \code{????.#...#... 4,1,1} - \emph{\code{16}} arrangements
\item \code{????.######..#####. 1,6,5} - \emph{\code{2500}} arrangements
\item \verb{?###???????? 3,2,1} - \emph{\code{506250}} arrangements
}

After unfolding, adding all of the possible arrangement counts together
produces \emph{\code{525152}}.

Unfold your condition records; \emph{what is the new sum of possible
arrangement counts?}
}
\examples{
f12a(example_data_12())
f12b()
}
