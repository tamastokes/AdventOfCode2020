# Not optimised for the number of computations,
# implemented this way to practice vector operations
# and higher-order functions
#
# TODO: next step could be to further generalize by replacing
# the hard-coded single / double for-loop with a recursize solution,
# but I think I'll stop here for now

shift.left <- function(v,n){
  if(n %% length(v) == 0)
    v
  else
    c(v[-(seq(1,n %% length(v)))],v[seq(1,n%%length(v))])
}

which.sum.to <- function(l, s){
  which(Reduce(`+`, l) == s)
}

mul.summing.to <- function(l, s){
  x <- which.sum.to(l, s)
  if (length(x) > 0){
    Reduce(`*`, l)[x]
  }
}

quiz.day1.1 <- function(v, s){
  result <- c()
  for (i in 1:length(v)){
    l <- list(v, shift.left(v,i))
    result <- c(result, mul.summing.to(l, s))
  }
  # double-counting occurs when vector is shifted by length(v)/2
  unique(result)
}

quiz.day1.1.test <- function(){
  v <- c(1721, 979, 366, 299, 675, 1456)
  s <- 2020
  quiz.day1.1(v, s)
}

quiz.day1.1.input <- function(){
  t <- read.table("./day1_input")
  v <- t$V1
  s <- 2020
  quiz.day1.1(v, s)
}

quiz.day1.2 <- function(v, s){
  result <- c()
  for (i in 1:length(v)){
    for (j in 1:length(v)){
      l <- list(v, shift.left(v,i), shift.left(v,j))
      result <- c(result, mul.summing.to(l, s))
    }
  }
  unique(result)
}

quiz.day1.2.test <- function(){
  v <- c(1721, 979, 366, 299, 675, 1456)
  s <- 2020
  quiz.day1.2(v, s)
}

quiz.day1.2.input <- function(){
  t <- read.table("./day1_input")
  v <- t$V1
  s <- 2020
  quiz.day1.2(v, s)
}
