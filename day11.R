check_occupied_north <- function(m,r,c){
  v <- m[(r-1):1,c]
  if (all(v >= 0)) return(FALSE)
  # there's at least one occupied
  if (all(v <= 0)) return(TRUE)
  # there are both empty and occupied seats, return that comes first
  which(v == -1)[1] < which(v == 1)[1]
}

check_occupied_south <- function(m,r,c){
  v <- m[(r+1):(dim(m)[1]),c]
  if (all(v >= 0)) return(FALSE)
  # there's at least one occupied
  if (all(v <= 0)) return(TRUE)
  # there are both empty and occupied seats, return that comes first
  which(v == -1)[1] < which(v == 1)[1]
}

check_occupied_west <- function(m,r,c){
  v <- m[r,(c-1):1]
  if (all(v >= 0)) return(FALSE)
  # there's at least one occupied
  if (all(v <= 0)) return(TRUE)
  # there are both empty and occupied seats, return that comes first
  which(v == -1)[1] < which(v == 1)[1]
}

check_occupied_east <- function(m,r,c){
  v <- m[r,(c+1):(dim(m)[2])]
  if (all(v >= 0)) return(FALSE)
  # there's at least one occupied
  if (all(v <= 0)) return(TRUE)
  # there are both empty and occupied seats, return that comes first
  which(v == -1)[1] < which(v == 1)[1]
}

check_occupied_northwest <- function(m,r,c){
  v <- rev(diag(m[max(1,r-c+1):(r-1),max(1,c-r+1):(c-1)]))
  if (all(v >= 0)) return(FALSE)
  # there's at least one occupied
  if (all(v <= 0)) return(TRUE)
  # there are both empty and occupied seats, return that comes first
  which(v == -1)[1] < which(v == 1)[1]
}

check_occupied_northeast <- function(m,r,c){
  v <- diag(t(m[(r-1):1,(c+1):(dim(m)[2])]))
  if (all(v >= 0)) return(FALSE)
  # there's at least one occupied
  if (all(v <= 0)) return(TRUE)
  # there are both empty and occupied seats, return that comes first
  which(v == -1)[1] < which(v == 1)[1]
}

check_occupied_southeast <- function(m,r,c){
  v <- diag(m[(r+1):(dim(m)[1]),(c+1):(dim(m)[2])])
  if (all(v >= 0)) return(FALSE)
  # there's at least one occupied
  if (all(v <= 0)) return(TRUE)
  # there are both empty and occupied seats, return that comes first
  which(v == -1)[1] < which(v == 1)[1]
}

check_occupied_southwest <- function(m,r,c){
  v <- diag(m[(r+1):(dim(m)[1]),(c-1):1])
  if (all(v >= 0)) return(FALSE)
  # there's at least one occupied
  if (all(v <= 0)) return(TRUE)
  # there are both empty and occupied seats, return that comes first
  which(v == -1)[1] < which(v == 1)[1]
}


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

flip2 <- function(m)
{
  nrows <- dim(m)[1]
  ncols <- dim(m)[2]
  n <- rep(0, nrows*ncols)
  dim(n) <- dim(m)
  for (i in 2:(nrows-1)){
    for (j in 2:(ncols-1)){
      # empty seats that see no occupied seats become occupied
      if (m[i,j] == 1){
        if (!any( check_occupied_northwest(m,i,j),
                  check_occupied_north(m,i,j),
                  check_occupied_northeast(m,i,j),
                  check_occupied_east(m,i,j),
                  check_occupied_southeast(m,i,j),
                  check_occupied_south(m,i,j),
                  check_occupied_southwest(m,i,j),
                  check_occupied_west(m,i,j)))
        {
          n[i,j] <- -1
        }
        else{
          n[i,j] <- 1
        }
      }
      # it now takes five or more visible occupied seats for an occupied seat to become empty
      if (m[i,j] == -1){
        v <- c(check_occupied_northwest(m,i,j),
                 check_occupied_north(m,i,j),
                 check_occupied_northeast(m,i,j),
                 check_occupied_east(m,i,j),
                 check_occupied_southeast(m,i,j),
                 check_occupied_south(m,i,j),
                 check_occupied_southwest(m,i,j),
                 check_occupied_west(m,i,j))
        if (sum(v) >= 5)
        {
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
#  m <- flip2(m)
 # m <- flip2(m)
  #m <- flip2(m)
  #return(m)
  while(TRUE){
    n <- flip2(m)
    print.m(n)
    if (all(m==n)) return(sum(n==-1))
    m <- n
  }
}