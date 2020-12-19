day10 <- function(filename){
  f <- read.table(filename)
  sorted <- sort(f$V1)
  diffs <- as.data.frame(Reduce(function(x,y){ c(y-x[2],y) }, sorted, accumulate = TRUE, init = c(0,sorted[1]) ))
  (sum(diffs[1,] == 1) + 1) * (sum(diffs[1,] == 3) + 1)
}

day10.2 <- function(filename){
  f <- read.table(filename)
  sorted <- c(-3,0,sort(f$V1))
  npaths <- c(1,1)
  for (i in 3:length(sorted)){
    path.to.this <- 0
    j <- 1
    while (sorted[i]-sorted[i-j] <= 3){
      path.to.this <- path.to.this + npaths[i-j]
      j <- j+1
    }
    npaths <- c(npaths, path.to.this)
  }
  npaths
}