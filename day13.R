day13 <- function(filename){
  now <- (read.table(filename,nrows = 1, as.is = TRUE))$V1
  f <- read.table(filename,skip=1,nrows=1,as.is = TRUE)
  v <- strsplit(f$V1,split = ',')
  v <- as.integer(v[[1]][v[[1]]!="x"])
  diff <- (v - (now %% v))
  min(diff) * v[which.min(diff)]
}

day13.2.slow <- function(filename){
  now <- (read.table(filename,nrows = 1, as.is = TRUE))$V1
  f <- read.table(filename,skip=1,nrows=1,as.is = TRUE)
  v <- strsplit(f$V1,split = ',')
  v <- v[[1]]
  v[v=="x"] <- 1
  v <- as.numeric(v)
  print(v)
  i <- v[1]
  while(TRUE){
    residuals <- (i+(1:(length(v)-1))) %% v[2:length(v)]
    #cat(i, residuals, "\n")
    if (all(residuals==0)) return(format(i,scientific = FALSE))
    if (i %% 100000 == 0) print(i)
    i <- i + v[1]
  }
}

find.first.match <- function(x,y,offset1){
  n <- x
  while(((n+offset1)%%y) != 0) n <- n+x
  n
}

day13.2 <- function(filename){
  now <- (read.table(filename,nrows = 1, as.is = TRUE))$V1
  f <- read.table(filename,skip=1,nrows=1,as.is = TRUE)
  v <- strsplit(f$V1,split = ',')
  v <- v[[1]]
  v[v=="x"] <- 1
  v <- as.numeric(v)
  print(v)
  
  left.multiplier <- v[1]
  left.offset <- 0
  for (i in 2:length(v)){
    if (v[i] == 1) next; # shortcut for performance reasons, but it should work without this too
    right.multiplier <- v[i]
    right.offset <- i-1
    cycle <- left.multiplier * right.multiplier
    first.match <- find.first.match(left.multiplier, right.multiplier, right.offset+left.offset)+left.offset
    cat(left.multiplier, left.offset, right.multiplier, right.offset, cycle, first.match, "\n")
    left.multiplier <- left.multiplier * right.multiplier
    left.offset <- first.match
  }
  format(first.match,scientific = FALSE)
}
