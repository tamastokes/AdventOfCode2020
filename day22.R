game <- function(a, b, count=1, level=1)
{
  if (count %% 100 == 0) cat("Game", count, "at level", level, " ", a, "  ", b, "\n")
  if (level > 1 && (length(a) %% 2 == 0) && (length(b) %% 2 == 0)){
    # Optimisation: unless there's an infinite loop (where player 1 wins),
    # the sub-game is always won by the player holding the highest number.
    # If there's an even number of cards in both players' hand, we won't get
    # into an infinite loop, so we can short-cut the sub-game.
    maxa <- max(a)
    maxb <- max(b)
    if (maxa > maxb && maxa > length(a)){
      cat("Shortcutting: 1 won\n")
      return(list(winner=1, hand=a, count=count))
    }
    if (maxb > maxa && maxb > length(b)){
      cat("Shortcutting: 2 won\n")
      return(list(winner=2, hand=b, count=count))
    }
  }
  counter <- count
  ma <- list()
  mb <- list()
  i <- 1
  while (length(a) > 0 & length(b) > 0) {
    #cat("a: ", a, "  b: ", b, "\n")
    # check if we're in an infinite loop
    if (length(ma)){
      va <- sapply(ma, identical, a)
      if (any(va)){
        j <- min(which(va==1))
        if (identical(mb[[j]], b)){
          #print("infinite loop")
          return(list(winner=1, hand=a, count=counter))
        }
      }
    }
    ma[[i]] <- a
    mb[[i]] <- b
    
    if ((a[1] <= (length(a)-1)) && (b[1] <= (length(b)-1))){
      x <- a[2:(a[1]+1)]
      y <- b[2:(b[1]+1)]
      #cat("starting subgame: ", x, "  ", y, "\n")
      counter <- counter + 1
      result <- game(x, y, counter, level+1)
      counter <- result$count
      #cat("player", result$winner, " won\n")
      winner <- result$winner
    }
    
    else{
      # ordinary round
      winner <- ifelse(a[1] > b[1], 1, 2)
    }
    
    if (winner == 1){
      a <- c(a[-1], a[1], b[1])
      b <- b[-1]
    }
    else{
      b <- c(b[-1], b[1], a[1])
      a <- a[-1]
    }
    i <- i+1
  }
  result <- list(winner=1, hand=a, count=counter)
  if (length(b) > 0){
    result$winner <- 2
    result$hand <- b
  }
  result
}

day22 <- function(){
  a <- c(10,
         21,
         37,
         2,
         47,
         13,
         6,
         29,
         9,
         3,
         4,
         48,
         46,
         25,
         44,
         41,
         23,
         20,
         24,
         12,
         45,
         43,
         5,
         27,
         50)
  b <- c(39,
         42,
         31,
         36,
         7,
         1,
         49,
         19,
         40,
         35,
         8,
         11,
         18,
         30,
         14,
         17,
         15,
         34,
         26,
         33,
         32,
         38,
         28,
         16,
         22)
  result <- game(a, b, 1, 1)
  print(result)
  sum(rev(result$hand) * (1:length(result$hand)))
}

day22.test <- function(){
  a <- c(9,
         2,
         6,
         3,
         1)
  b <- c(5,
         8,
         4,
         7,
         10)
  result <- game(a, b, 1, 1)
  print(result)
  sum(rev(result$hand) * (1:length(result$hand)))
}

day22.infinite.loop.test <- function(){
  a <- c(43,19)
  b <- c(2,29,14)
  game(a, b, 1, 1)
}