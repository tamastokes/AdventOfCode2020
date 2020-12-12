#calc.binary <- function(s, start, stop, ch.one)
#{
#  n <- 0
#  for (i in start:stop){
#    n <- n * 2
#    if (substr(s,i,i) == ch.one){
#      n <- n + 1
#    }
#  }
#  n
#}

day5 <- function(filename){
  t <- read.table(filename, header=FALSE, as.is=TRUE)
  max.seat.id <- 0
  v <- rep(0,2^10)
  for (l in t$V1)
  {
    #row <- calc.binary(l, 1, 7, "B")
    #col <- calc.binary(l, 8, 10, "R")
    row <- Reduce(function(x,y){ x <- x*2; if(y == "B") x <- x+1; x }, substring(l,1:7,1:7), 0)
    col <- Reduce(function(x,y){ x <- x*2; if(y == "R") x <- x+1; x }, substring(l,8:10,8:10), 0)
    seat.id <- row * 8 + col
    v[seat.id] <- 1
    if (seat.id > max.seat.id) max.seat.id <- seat.id
  }
  # solution to 1st part
  max.seat.id
  # solution to 2nd part
  intersect(setdiff(which(v==0),which(v==0)-1),setdiff(which(v==0),which(v==0)+1))
  #dim(v) = c(8,128)
  #t(v)
}
