# Not optimised for the number of computations,
# implemented this way to practice vector operations
# and higher-order functions

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

quiz.day1 <- function(l, s){
  result <- c()
  d <- length(l)
  n <- length(l[[1]])
  t <- n^d
  for (i in 1:(t)){
    result <- c(result, mul.summing.to(l, s))
    # rotate the vectors (similarly to n-base counting)
    if (i<t){
      # calculate how many vectors we need to rotate at this step
      m <- i
      cnt <- 0
      while (m %% n == 0){
        m <- m / n
        cnt <- cnt + 1
      }
      # execute the rotation for the necessary vectors
      for (j in (d-cnt):d){
        l[[j]] <- shift.left(l[[j]],1)
      }
    }
  }
  # double-counting typiccally occurs when a vector is
  # added to itself shifted by length(v)/2
  unique(result)
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
  l <- list(v,v)
  print(quiz.day1.1(v, s))
  print(quiz.day1(l, s))
}

quiz.day1.1.input <- function(){
  t <- read.table("./day1_input")
  v <- t$V1
  l <- list(v, v)
  s <- 2020
  print(quiz.day1.1(v, s))
  print(quiz.day1(l, s))
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
  l <- list(v,v,v)
  print(quiz.day1.2(v, s))
  print(quiz.day1(l, s))
}

quiz.day1.2.input <- function(){
  t <- read.table("./day1_input")
  v <- t$V1
  l <- list(v,v,v)
  s <- 2020
  print(quiz.day1.2(v, s))
  print(quiz.day1(l, s))
}
