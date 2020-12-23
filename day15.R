day15 <- function(){
  v <- c(0,3,6)
#  v <- c(0,6,1,7,2,19,20)
  for (i in (length(v)+1):100)
  {
    last <- tail(v, 1)
    h <- head(v, -1)
    occurences <- which(h==last)
    v <- c(v, ifelse(length(occurences) > 0, (i-1)-max(occurences), 0))
  }
  tail(v,1)
  v
}

# this is still too slow, but not algorithmically, but because of the list handling of R.
# maybe it could be faster if I could figure out how to make R store list of integers not as vectors,
# which would occupy too much space in this case.
day15.faster <- function(){
  v <- c(0,3,6)
  #v <- c(0,6,1,7,2,19,20)
  l <- list()
  for (i in 1:length(v)) l[[as.character(v[i])]] <- c(0,i)
  last <- tail(v,1)
  
  for (i in (length(v)+1):30000000)
  {
    occurance <- l[[as.character(last)]]
    if (occurance[1] == 0){
      stored <- l[["0"]]
      if(is.null(stored)) stored <- c(0,0)
      l[["0"]] <- c(stored[2],i)
      last <- 0
    }
    else{
      diff <- (i-1)-as.numeric(occurance[1])
      stored <- l[[as.character(diff)]]
      if(is.null(stored)) stored <- c(0,0)
      l[[as.character(diff)]] <- c(stored[2],i)
      last <- diff
    }
    if (i %% 1000 == 0) cat(i, length(l),"\n")
  }
  last
}
