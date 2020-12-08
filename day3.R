parseInput <- function(filename){
  t <- read.table(filename,as.is=TRUE,comment.char = "")
  v <- NULL
  for (l in t$V1){
    for (i in 1:nchar(l)){
      ch <- substr(l,i,i)
      v <- c(v, ifelse(ch=='#', 1, 0))
    }
  }
  # R is column-based, so we'll need to transpose
  dim(v) <- c(nchar(t$V1[1]), length(t$V1))
  t(v)
}

get.ntrees.for.slope <- function(m, nright, ndown, nrows, ncols)
{
  rows <- seq(1, length.out=nrows/ndown, by=ndown)
  cols <- seq(1, length.out=length(rows), by=nright) %% ncols
  cols[cols==0] <- ncols
  # again, went for a one-liner rather-than a computationally-optimized solution here
  sum(tail(diag(m[rows,cols]),-1))
}

day3.1 <- function(){
  m <- parseInput("day3_input")
  nrows <- dim(m)[1]
  ncols <- dim(m)[2]
  get.ntrees.for.slope(m, 3,1, nrows, ncols)
}

day3.2 <- function(){
  m <- parseInput("day3_input")
  nrows <- dim(m)[1]
  ncols <- dim(m)[2]
  
  v <- NULL
  v <- c(v, get.ntrees.for.slope(m, 1,1, nrows, ncols))
  v <- c(v, get.ntrees.for.slope(m, 3,1, nrows, ncols))
  v <- c(v, get.ntrees.for.slope(m, 5,1, nrows, ncols))
  v <- c(v, get.ntrees.for.slope(m, 7,1, nrows, ncols))
  v <- c(v, get.ntrees.for.slope(m, 1,2, nrows, ncols))
  prod(v)
}

