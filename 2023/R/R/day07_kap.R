if (FALSE) {
  
  # p← io:read "p07.txt"
  p <- ex(7)
  # p <- input(7)
  # ⍝p← io:read "../R/inst/input07.txt"
  # p←⊃{" " regex:split ⍵}¨p
  p <- matrix(unlist(strsplit(p, " ")), ncol = 2, byrow = TRUE)
  p
  # hands←⊣/p
  hands <- p[,1]
  hands
  # bids←⍎¨⊢/p
  bids <- as.integer(p[,2])
  bids
  # key⇐⊢«,»(⍪+⌿≡⌻)∘∪
  key <- function(x) {
    l <- strsplit(x, "")[[1]]
    setNames(colSums(outer(l, unique(l), "==")), unique(l))
  }
  hands[2]
  y <- strsplit(hands[2], "")[[1]]
  outer(y, unique(y), "==")
  setNames(colSums(outer(y, unique(y), "==")), unique(y))
  sapply(hands, key)
  # also 
  sapply(strsplit(hands, ""), table)
  # handrank⇐{10⊥2↑∨⊢/key ⍵}
  handrank <- function(x) {
    rank <- sort(sapply(x, key), decreasing = TRUE)
    if (length(rank) == 1) rank <- c(rank, 0)
    as.integer(paste(rank[1:2], collapse = ""))
  }
  sapply(hands, handrank)
  # ranks←"23456789TJQKA"
  ranks <- c(2:9, "T", "J", "Q", "K", "A")
  # ans←+/ranks{bids×1+⍋⍋⍵,⍺⍳⊃hands}handrank¨hands
  sortrank <- function(x, y) {
    m <- matrix(strsplit(paste0(y, collapse = ""), "")[[1]], ncol = 5, byrow = TRUE)
    mm <- matrix(match(m, x), ncol = 5, byrow = FALSE)
    g <- cbind(sapply(y, handrank), mm)
    do.call(order, as.data.frame(g))
  }
  m <- matrix(strsplit(paste0(hands, collapse = ""), "")[[1]], ncol = 5, byrow = TRUE)
  mm <- matrix(match(m, ranks), ncol = 5, byrow = FALSE)
  g <- cbind(sapply(hands, handrank), mm)
  gdf <- as.data.frame(g)
  gdf[do.call(order, gdf), ]
  sortrank(ranks, hands)
  # ans
  sum(bids*sortrank(ranks, hands)) # wrong
  sum(bids[sortrank(ranks, hands)]*seq_along(bids))

  
  
  key <- function(x) {
    l <- strsplit(x, "")[[1]]
    setNames(colSums(outer(l, unique(l), "==")), unique(l))
  }
  
  # handrank <- function(x) {
  #   rank <- sort(sapply(x, key), decreasing = TRUE)
  #   if (length(rank) == 1) rank <- c(rank, 0)
  #   as.integer(paste(rank[1:2], collapse = ""))
  # }
  handrank <- function(x) {
    rank <- sort(sapply(strsplit(x, ""), table), decreasing = TRUE)
    if (length(rank) == 1) rank <- c(rank, 0)
    as.integer(paste(rank[1:2], collapse = ""))
  }
  
  sortrank <- function(x, y) {
    m <- matrix(strsplit(paste0(y, collapse = ""), "")[[1]], ncol = 5, byrow = TRUE)
    mm <- matrix(match(m, x), ncol = 5, byrow = FALSE)
    g <- cbind(sapply(y, handrank), mm)
    do.call(order, as.data.frame(g))
  }
  
  solve <- function(x) {
    p <- matrix(unlist(strsplit(x, " ")), ncol = 2, byrow = TRUE)
    hands <- p[,1]
    bids <- as.integer(p[,2])
    ranks <- c(2:9, "T", "J", "Q", "K", "A")
    sum(bids[sortrank(ranks, hands)]*seq_along(bids))
  }
  solve(readLines("~/Projects/advent-of-code/2023/R/inst/example07.txt"))
  solve(input(7))
  
}
