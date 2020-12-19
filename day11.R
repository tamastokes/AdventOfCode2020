flip <- function(m)
{
  nrows <- dim(m)[1]
  ncols <- dim(m)[2]
  n <- rep(0, nrows*ncols)
  dim(n) <- dim(m)
  for (i in 2:(nrows-1)){
    for (j in 2:(ncols-1)){
      # If a seat is empty (L) and there are no occupied seats adjacent to it, the seat becomes occupied.
      if (m[i,j] == 1){
        if (all(m[(i-1):(i+1),(j-1):(j+1)]>=0)){ # m[i,j] == 1, so it's ok
          n[i,j] <- -1
        }
        else{
          n[i,j] <- 1
        }
      }
      # If a seat is occupied (#) and four or more seats adjacent to it are also occupied, the seat becomes empty.
      if (m[i,j] == -1){
        if (sum( c(m[i-1,(j-1):(j+1)], m[i+1,(j-1):(j+1)], m[i,j-1], m[i,j+1])==-1 ) >=4 ){
          n[i,j] <- 1
        }
        else{
          n[i,j] <- -1
        }
      }
    }
  }
  n
}

print.m <- function(m)
{
  nrows <- dim(m)[1]
  ncols <- dim(m)[2]
  for (i in 2:(nrows-1)){
    for (j in 2:(ncols-1)){
      if (m[i,j] == -1){
        cat('#')
      }
      if (m[i,j] == 0){
        cat('.')
      }
      if (m[i,j] == 1){
        cat('L')
      }
    }
    cat('\n')
  }
  cat('\n')
}

day11 <- function(filename){
  f <- read.table(filename, as.is=TRUE)
  nrows <- length(f$V1)
  ncols <- nchar(f$V1[1])
  m <- rep(0, nrows*ncols)
  dim(m) <- c(nrows,ncols)
  for (i in seq(nrows)){
    for (j in seq(ncols))
    {
      ch <- substr(f$V1[i],j,j)
      if(ch=="L") m[i,j] <- 1
    }
  }
  m <- cbind(rep(0,nrows),m)
  m <- cbind(m,rep(0,nrows))
  m <- rbind(rep(0,ncols+2),m)
  m <- rbind(m,rep(0,ncols+2))
  while(TRUE){
    n <- flip(m)
    if (all(m==n)) return(sum(n==-1))
    m <- n
  }
}