day10 <- function(filename){
  f <- read.table(filename)
  sorted <- sort(f$V1)
  diffs <- as.data.frame(Reduce(function(x,y){ c(y-x[2],y) }, sorted, accumulate = TRUE, init = c(0,sorted[1]) ))
  (sum(diffs[1,] == 1) + 1) * (sum(diffs[1,] == 3) + 1)
}