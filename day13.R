day13 <- function(filename){
  now <- (read.table(filename,nrows = 1, as.is = TRUE))$V1
  f <- read.table(filename,skip=1,nrows=1,as.is = TRUE)
  v <- strsplit(f$V1,split = ',')
  v <- as.integer(v[[1]][v[[1]]!="x"])
  diff <- (v - (now %% v))
  min(diff) * v[which.min(diff)]
}