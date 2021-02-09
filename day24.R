calculate.path <- function(tile.path, start.pos){
  x <- start.pos[1]
  y <- start.pos[2]
  i <- 1
  while (i <= nchar(tile.path)){
    if (substr(tile.path, i, i) == 's'){
      if (substr(tile.path, i+1, i+1) == 'w'){
        x <- x - 1
      }
      else{
        x <- x + 1
      }
      y <- y + 1
      i <- i + 1
    }
    else if (substr(tile.path, i, i) == 'n'){
      if (substr(tile.path, i+1, i+1) == 'w'){
        x <- x - 1
      }
      else{
        x <- x + 1
      }
      y <- y - 1
      i <- i + 1
    }
    else if (substr(tile.path, i, i) == 'w'){
      x <- x - 2
    }
    else{
      x <- x + 2
    }
    i <- i + 1
  }
  c(x,y)
}

flip.tile <- function(floor, tile.path){
  xy <- calculate.path(tile.path, dim(floor)/2)
  floor[xy[1],xy[2]] = 1 - floor[xy[1],xy[2]]
  floor
}

day24 <- function(filename)
{
  floor <- matrix(data=0, nrow=200, ncol=200)
  f <- read.table(filename, header=FALSE, as.is=TRUE, blank.lines.skip=FALSE)
  for (v in f$V1){
    floor <- flip.tile(floor, v)
  }
  sum(floor)
}