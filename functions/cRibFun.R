library(tidyverse)

suit <- c("S", "H", "C", "D")
value <- c("A", 2:10, "J", "Q", "K")
numvalue <- c(1:10, rep(10, 3))
seqvalue <- 1:13
names(numvalue) <- names(seqvalue) <- value

deck <- lapply(value, function(x){
    paste(rep(x, 4), suit, sep = "")
  }) %>%
    unlist()

Suit <- function(x){
  substr(x, nchar(x), nchar(x))
}

Number <- function(x){
  substr(x, 1, nchar(x)-1)
}


CribCount <- function(kh, flop, DEALER = FALSE, NOBBINS = TRUE, FLUSH = TRUE){
  hand <- append(kh, flop)
  fc <- numvalue[substr(hand, 1,nchar(hand)-1)]
  #####Two for his nobs
  twoforhisnobsPoints <- ifelse(DEALER && grepl("J", flop), 2, 0)
  
  
  #####Nobbins
  nobbinsPoints <- 0
  if(NOBBINS && any(grepl("J", kh)))
    nobbinsPoints <- any(substr(kh[grepl("J", kh)], 2, 2) == substr(flop, nchar(flop), nchar(flop)))
  
  
  #####Calculating a Run
  tsv <- seqvalue[substr(hand, 1,nchar(hand)-1)]
  runPoints <- 5 * tsv %>%
    sort() %>%
    diff() %>%
    all(. == 1)
  
  if(runPoints == 0){
    runPoints <- 4 * combn(tsv, 4) %>%
      apply(2, sort) %>%
      apply(2, diff) %>%
      apply(2, function(x)all(x == 1)) %>%
      sum()
    if(runPoints == 0){
      runPoints <- 3 * combn(tsv, 3) %>%
        apply(2, sort) %>%
        apply(2, diff) %>%
        apply(2, function(x)all(x == 1)) %>%
        sum()
    }
  }
  
  #####Calculating pairs
  pairPoints <- 2 * combn(seqvalue[substr(hand, 1,nchar(hand)-1)], 2) %>%
    apply(., 2, function(x)x[1] == x[2]) %>%
    sum(.)
  
  
  #####Calculate suit
  
  if(FLUSH){
    stab <- substr(kh, nchar(kh), nchar(kh)) %>%
      table(.)
    
    suitPoints <- ifelse(any(stab == 4), 4, 0)
    
    if(suitPoints == 4 && substr(flop, nchar(flop), nchar(flop)) == substr(kh[1], nchar(kh[1]), nchar(kh[1])))
      suitPoints <- suitPoints + 1
  }else suitPoints <- 0

  
  #####Calculate 15s
  AllComb <- lapply(2:5, function(count){
    combn(fc, count)
  }) 
  
  fifFinder <- data.frame(Empty = 2:5) %>%
    group_by(Empty) %>%
    nest() %>%
    mutate(Combs = map(Empty, function(count){
      combn(fc, count)
    }),
    CombsSum = map(Combs, colSums),
    N15 = map_int(CombsSum, ~sum(.x == 15))) 

  
  fifPoints <- 2 * sum(fifFinder$N15)
  
  
  
  #####sum the points
  totalPoints <- twoforhisnobsPoints + nobbinsPoints + runPoints + pairPoints + suitPoints + fifPoints
  
  
  totalPoints
}


