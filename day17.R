
print.a <- function(a)
{
  nrows <- dim(a)[1]
  ncols <- dim(a)[2]
  nheight <- dim(a)[3]

    for (h in 1:nheight){
    for (i in 1:nrows){
      for (j in 1:ncols){
        if (a[i,j,h] == 1){
          cat('#')
        }
        if (a[i,j,h] == 0){
          cat('.')
        }
      }
      cat('\n')
    }
    cat('\n')
  }
  cat('\n')
}

expand.matrix <- function(m){
  nrows <- dim(m)[1]
  ncols <- dim(m)[2]
  m <- cbind(rep(0,nrows),m)
  m <- cbind(m,rep(0,nrows))
  m <- rbind(rep(0,ncols+2),m)
  m <- rbind(m,rep(0,ncols+2))
  m
}

expand.cube <- function(a){
  dims <- dim(a)+2
  v <- rep(0,dims[1]*dims[2])
  for (h in 1:(dim(a)[3])){
    v <- c(v, expand.matrix(a[,,h,,drop=TRUE]))
  }
  v <- c(v, rep(0,dims[1]*dims[2]))
  dim(v) <- head(dims,3)
  v
}

#TODO: generalize this and the two above
expand.4d <- function(a){
  dims <- dim(a)+2
  v <- rep(0,dims[1]*dims[2]*dims[3])
  for (hh in 1:(dim(a)[4])){
    v <- c(v, expand.cube(a[,,,hh,drop=FALSE]))
  }
  v <- c(v, rep(0,dims[1]*dims[2]*dims[3]))
  dim(v) <- dims
  v
}

calc.activations <- function(a){
  nx <- dim(a)[1]
  ny <- dim(a)[2]
  nz <- dim(a)[3]
  nw <- dim(a)[4]
  b <- array(0, dim(a))
  for (w in 2:(nw-1)){
    for (z in 2:(nz-1)){
      for (x in 2:(nx-1)){
        for (y in 2:(ny-1)){
          neighbours <- a[(x-1):(x+1),(y-1):(y+1),(z-1):(z+1),(w-1):(w+1)]
          #cat(x,y,z, " ", a[x,y,z], " ", sum(neighbours),  "\n")
          #print(array(neighbours,c(3,3,3)))
          if (a[x,y,z,w] == 1){
            if (sum(neighbours) == 3 | sum(neighbours) == 4){ # sum contains the node itself!
              b[x,y,z,w] = 1
            }
            else{
              b[x,y,z,w] = 0
            }
          }
          else {
            if (sum(neighbours) == 3){
              b[x,y,z,w] = 1
            }
            else{
              b[x,y,z,w] = 0
            }
          }
        }
      }
    }
  }
  b
}

day17 <- function(filename){
  f <- read.table(filename, as.is=TRUE, comment.char = "")
  nrows <- length(f$V1)
  ncols <- nchar(f$V1[1])
  a <- rep(0, nrows*ncols)
  dim(a) <- c(nrows,ncols,1,1)
  for (i in seq(nrows)){
    for (j in seq(ncols))
    {
      ch <- substr(f$V1[i],j,j)
      if(ch=="#") a[i,j,1,1] <- 1
    }
  }
  a <- expand.4d(a)
  a <- expand.4d(a)
  #print.a(a)
  a <- calc.activations(a)
  a <- expand.4d(a)
  a <- calc.activations(a)
  a <- expand.4d(a)
  a <- calc.activations(a)
  a <- expand.4d(a)
  a <- calc.activations(a)
  a <- expand.4d(a)
  a <- calc.activations(a)
  a <- expand.4d(a)
  a <- calc.activations(a)
  #print.a(a)
  sum(a)
}