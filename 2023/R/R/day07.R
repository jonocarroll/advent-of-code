#' Day 07: Camel Cards
#'
#' [Camel Cards](https://adventofcode.com/2023/day/7)
#'
#' @name day07
#' @rdname day07
#' @details
#'
#' **Part One**
#'
#' Your all-expenses-paid trip turns out to be a one-way, five-minute ride
#' in an [airship](https://en.wikipedia.org/wiki/Airship){target="_blank"}.
#' (At least it\'s a [*cool*
#' airship]{title="Please only read this sentence while listening to 'The Airship Blackjack' from the Final Fantasy 6 soundtrack."}!)
#' It drops you off at the edge of a vast desert and descends back to
#' Island Island.
#' 
#' \"Did you bring the parts?\"
#' 
#' You turn around to see an Elf completely covered in white clothing,
#' wearing goggles, and riding a large
#' [camel](https://en.wikipedia.org/wiki/Dromedary){target="_blank"}.
#' 
#' \"Did you bring the parts?\" she asks again, louder this time. You
#' aren\'t sure what parts she\'s looking for; you\'re here to figure out
#' why the sand stopped.
#' 
#' \"The parts! For the sand, yes! Come with me; I will show you.\" She
#' beckons you onto the camel.
#' 
#' After riding a bit across the sands of Desert Island, you can see what
#' look like very large rocks covering half of the horizon. The Elf
#' explains that the rocks are all along the part of Desert Island that is
#' directly above Island Island, making it hard to even get there.
#' Normally, they use big machines to move the rocks and filter the sand,
#' but the machines have broken down because Desert Island recently stopped
#' receiving the *parts* they need to fix the machines.
#' 
#' You\'ve already assumed it\'ll be your job to figure out why the parts
#' stopped when she asks if you can help. You agree automatically.
#' 
#' Because the journey will take a few days, she offers to teach you the
#' game of *Camel Cards*. Camel Cards is sort of similar to
#' [poker](https://en.wikipedia.org/wiki/List_of_poker_hands){target="_blank"}
#' except it\'s designed to be easier to play while riding a camel.
#' 
#' In Camel Cards, you get a list of *hands*, and your goal is to order
#' them based on the *strength* of each hand. A hand consists of *five
#' cards* labeled one of `A`, `K`, `Q`, `J`, `T`, `9`, `8`, `7`, `6`, `5`,
#' `4`, `3`, or `2`. The relative strength of each card follows this order,
#' where `A` is the highest and `2` is the lowest.
#' 
#' Every hand is exactly one *type*. From strongest to weakest, they are:
#' 
#' -   *Five of a kind*, where all five cards have the same label: `AAAAA`
#' -   *Four of a kind*, where four cards have the same label and one card
#'     has a different label: `AA8AA`
#' -   *Full house*, where three cards have the same label, and the
#'     remaining two cards share a different label: `23332`
#' -   *Three of a kind*, where three cards have the same label, and the
#'     remaining two cards are each different from any other card in the
#'     hand: `TTT98`
#' -   *Two pair*, where two cards share one label, two other cards share a
#'     second label, and the remaining card has a third label: `23432`
#' -   *One pair*, where two cards share one label, and the other three
#'     cards have a different label from the pair and each other: `A23A4`
#' -   *High card*, where all cards\' labels are distinct: `23456`
#' 
#' Hands are primarily ordered based on type; for example, every *full
#' house* is stronger than any *three of a kind*.
#' 
#' If two hands have the same type, a second ordering rule takes effect.
#' Start by comparing the *first card in each hand*. If these cards are
#' different, the hand with the stronger first card is considered stronger.
#' If the first card in each hand have the *same label*, however, then move
#' on to considering the *second card in each hand*. If they differ, the
#' hand with the higher second card wins; otherwise, continue with the
#' third card in each hand, then the fourth, then the fifth.
#' 
#' So, `33332` and `2AAAA` are both *four of a kind* hands, but `33332` is
#' stronger because its first card is stronger. Similarly, `77888` and
#' `77788` are both a *full house*, but `77888` is stronger because its
#' third card is stronger (and both hands have the same first and second
#' card).
#' 
#' To play Camel Cards, you are given a list of hands and their
#' corresponding *bid* (your puzzle input). For example:
#' 
#'     32T3K 765
#'     T55J5 684
#'     KK677 28
#'     KTJJT 220
#'     QQQJA 483
#' 
#' This example shows five hands; each hand is followed by its *bid*
#' amount. Each hand wins an amount equal to its bid multiplied by its
#' *rank*, where the weakest hand gets rank 1, the second-weakest hand gets
#' rank 2, and so on up to the strongest hand. Because there are five hands
#' in this example, the strongest hand will have rank 5 and its bid will be
#' multiplied by 5.
#' 
#' So, the first step is to put the hands in order of strength:
#' 
#' -   `32T3K` is the only *one pair* and the other hands are all a
#'     stronger type, so it gets rank *1*.
#' -   `KK677` and `KTJJT` are both *two pair*. Their first cards both have
#'     the same label, but the second card of `KK677` is stronger (`K` vs
#'     `T`), so `KTJJT` gets rank *2* and `KK677` gets rank *3*.
#' -   `T55J5` and `QQQJA` are both *three of a kind*. `QQQJA` has a
#'     stronger first card, so it gets rank *5* and `T55J5` gets rank *4*.
#' 
#' Now, you can determine the total winnings of this set of hands by adding
#' up the result of multiplying each hand\'s bid with its rank (`765` \*
#' 1 + `220` \* 2 + `28` \* 3 + `684` \* 4 + `483` \* 5). So the *total
#' winnings* in this example are *`6440`*.
#' 
#' Find the rank of every hand in your set. *What are the total winnings?*
#'
#' **Part Two**
#' 
#' To make things a little more interesting, the Elf introduces one
#' additional rule. Now, `J` cards are
#' [jokers](https://en.wikipedia.org/wiki/Joker_(playing_card)){target="_blank"} -
#' wildcards that can act like whatever card would make the hand the
#' strongest type possible.
#' 
#' To balance this, *`J` cards are now the weakest* individual cards,
#' weaker even than `2`. The other cards stay in the same order: `A`, `K`,
#' `Q`, `T`, `9`, `8`, `7`, `6`, `5`, `4`, `3`, `2`, `J`.
#' 
#' `J` cards can pretend to be whatever card is best for the purpose of
#' determining hand type; for example, `QJJQ2` is now considered *four of a
#' kind*. However, for the purpose of breaking ties between two hands of
#' the same type, `J` is always treated as `J`, not the card it\'s
#' pretending to be: `JKKK2` is weaker than `QQQQ2` because `J` is weaker
#' than `Q`.
#' 
#' Now, the above example goes very differently:
#' 
#'     32T3K 765
#'     T55J5 684
#'     KK677 28
#'     KTJJT 220
#'     QQQJA 483
#' 
#' -   `32T3K` is still the only *one pair*; it doesn\'t contain any
#'     jokers, so its strength doesn\'t increase.
#' -   `KK677` is now the only *two pair*, making it the second-weakest
#'     hand.
#' -   `T55J5`, `KTJJT`, and `QQQJA` are now all *four of a kind*! `T55J5`
#'     gets rank 3, `QQQJA` gets rank 4, and `KTJJT` gets rank 5.
#' 
#' With the new joker rule, the total winnings in this example are
#' *`5905`*.
#' 
#' Using the new joker rule, find the rank of every hand in your set. *What
#' are the new total winnings?*
#'
#' @param x some data
#' @return For Part One, `f07a(x)` returns .... For Part Two,
#'   `f07b(x)` returns ....
#' @export
#' @examples
#' f07a(example_data_07())
#' f07b()
f07a <- function(x) {
  xx <- strsplit(x, " ")
  hands <- purrr::map_chr(xx, 1)
  bids <- as.integer(purrr::map_chr(xx, 2))
  sum(bids[order(sapply(hands, strength))]*1:length(hands))
}


#' @rdname day07
#' @export
f07b <- function(x) {
  xx <- strsplit(x, " ")
  hands <- purrr::map_chr(xx, 1)
  bids <- as.integer(purrr::map_chr(xx, 2))
  sum(bids[order(sapply(hands, strength_2))]*1:length(hands))
}


f07_helper <- function(x) {

}

strength <- function(x) {
  detect_5oak(x) +
    detect_4oak(x) +
    detect_fh(x) +
    detect_3oak(x) +
    detect_2p(x) +
    detect_1p(x) +
    detect_hk(x) 
}

strength_2 <- function(x) {
  repcards <- c("A", "K", "Q", "T", 9:2)
  w <- vector("integer", length(repcards))
  for (i in seq_along(repcards)) {
    subcard <- gsub("J", repcards[i], x)
    if (detect_5oak(subcard) > 0) w[i] <- 900000000
    if (detect_4oak(subcard) > 0) w[i] <- 800000000
    if (detect_fh(subcard) > 0) w[i] <- 700000000
    if (detect_3oak(subcard) > 0) w[i] <- 600000000
    if (detect_2p(subcard) > 0) w[i] <- 500000000
    if (detect_1p(subcard) > 0) w[i] <- 400000000
    if (detect_hk(subcard) > 0) w[i] <- 300000000
  }
  max(w) + sum(20^(5:1)*score_card(into_cards(x), part = "b"))
}

score_card <- function(x, part = "a") {
  if (part == "a") {
    cards <- rev(c("A", "K", "Q", "J", "T", 9:2))
    values <- rev(c("e", "d", "c", "b", "a", 9:2))
  } else {
    cards <- rev(c("A", "K", "Q", "T", 9:2, "J"))
    values <- rev(c("d", "c", "b", "a", 9:1))
  }
  as.hexmode(values[match(x, cards)])
}

detect_5oak <- function(x) {
  cards <- into_cards(x)
  if (!length(unique(cards)) == 1) return(0)
  900000000 + sum(20^(5:1)*score_card(cards))
}

detect_4oak <- function(x) {
  cards <- into_cards(x)
  if (!any(table(cards) == 4)) return(0)
  800000000 + sum(20^(5:1)*score_card(cards))
}

detect_fh <- function(x) {
  cards <- into_cards(x)
  tab <- table(cards)
  if (!setequal(tab, c(3, 2))) return(0)
  700000000 + sum(20^(5:1)*score_card(cards))
}

detect_3oak <- function(x) {
  cards <- into_cards(x)
  tab <- table(cards)
  if (!setequal(tab, c(3, 1))) return(0)
  600000000 + sum(20^(5:1)*score_card(cards))
}

detect_2p <- function(x) {
  cards <- into_cards(x)
  tab <- table(cards)
  if (!length(tab[tab==2]) == 2) return(0)
  500000000 + sum(20^(5:1)*score_card(cards))
}

detect_1p <- function(x) {
  cards <- into_cards(x)
  tab <- table(cards)
  if (!(length(tab[tab==2]) == 1 && length(tab[tab==1]) == 3)) return(0)
  400000000 + sum(20^(5:1)*score_card(cards))
}

detect_hk <- function(x) {
  cards <- into_cards(x)
  tab <- table(cards)
  if (!length(tab[tab==1]) == 5) return(0)
  sum(20^(5:1)*score_card(cards))
}

into_cards <- function(x) {
  strsplit(x, "")[[1]]  
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day07
#' @export
example_data_07 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
