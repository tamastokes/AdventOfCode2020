shift.left <- function(v,n){
  if(n %% length(v) == 0)
    v
  else
    c(v[-(seq(1,n %% length(v)))],v[seq(1,n%%length(v))])
}

get.first.wrong <- function(f, n){
  for (i in 1:(length(f$V1)-n-1)){
    v <- f$V1[i:(i+n)]
    ok <- FALSE
    for (j in 1:n)
    {
      w <- shift.left(v, j)
      if ( any(v+w == f$V1[i+n+1])){
        ok <- TRUE
        break
      }
    }
    if(!ok) return(f$V1[i+n+1])
  }
}

day9.1 <- function(filename, n){
  f <- read.table(filename)
  get.first.wrong(f, n)
}

day9.2 <- function(filename, n){
  f <- read.table(filename)
  val <- get.first.wrong(f, n)
  from <- 1
  to <- 2
  while(TRUE){
    s <- sum(f$V1[from:to])
    if (val == s){
      return( min(f$V1[from:to]) + max(f$V1[from:to]))
    }
    if (val > s)
    {
      from <- from+1
    }
    else{
      to <- to+1
    }
  }
}
