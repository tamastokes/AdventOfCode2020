parseInput <- function(filename){
  t <- read.table(filename,as.is=TRUE)
  l <- unlist(lapply(strsplit(t$V1,'-'), strtoi))
  min <- l[seq(1,length(l),2)]
  max <- l[seq(2,length(l),2)]
  d <- data.frame(min, max)
  d$ch <- substr(t$V2,1,1)
  d$pwd <- t$V3
  d
}

vectorize.str <- function(s){
  n <- nchar(s)
  v <- vector()
  for (i in 1:nchar(s)){
    v[i] <- substr(s, i, i)
  }
  v
}

check.valid.1 <- function(l){
  # TODO: not very happy with this solution yet: l has lost all its structure
  # (because of the fact that this function is called by 'apply'), and now it's
  # a vector of 4 strings
  v <- vectorize.str(l[[4]])
  s <- sum(l[[3]] == v)
  min <- strtoi(l[[1]])
  max <- strtoi(l[[2]])
  valid <- ifelse((min<=s) & (s<=max), TRUE, FALSE)
  #cat(min, max, s, valid, l[[3]], l[[4]], "\n")
  valid
}

day2.1 <- function(){
  d <- parseInput("day2_input")
  v <- apply(d, 1, check.valid.1)
  sum(v)
}
