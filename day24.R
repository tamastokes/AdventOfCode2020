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

flip.tile <- function(floor, tile.path, start.tile){
  xy <- calculate.path(tile.path, start.tile)
  floor[xy[2],xy[1]] = 1 - floor[xy[2],xy[1]]
  floor
}

black.neighbours <- function(floor, row, col){
  sum(floor[(row-1),(col-1):(col+1)]) + sum(floor[(row),(col-2):(col+2)]) + sum(floor[(row+1),(col-1):(col+1)])
}

is.real.tile <- function(floor, row, col, start.tile){
  row.diff <- abs(start.tile[1] - row)
  col.diff <- abs(start.tile[2] - col)
  if (row.diff %% 2 == 0){
    if (col.diff %% 2 == 0){
      return(TRUE)
    }
  }
  else{
    if (col.diff %% 2 == 1){
      return(TRUE)
    }
  }
  return(FALSE)
}

next.day <- function(floor, start.tile){
  floor.new <- floor
  blacks <- which(floor == 1, arr.ind=TRUE, useNames = FALSE)
  for (i in 1:(dim(blacks)[1])){
    row <- blacks[i,1]
    col <- blacks[i,2]
    black.neighbours <- black.neighbours(floor, row, col) - 1 # self is also counted, hence -1!
    if (black.neighbours == 0 || black.neighbours > 2){
      floor.new[row, col] <- 0
    }
  }
  corners <- find.useful.range(floor)
  row.range <- corners[1]:corners[3]
  col.range <- corners[2]:corners[4]
  floor.lim <- floor[row.range,col.range]
  whites <- which(floor.lim == 0, arr.ind=TRUE, useNames = FALSE)
  whites[,1] <- whites[,1] + (corners[1]-1)
  whites[,2] <- whites[,2] + (corners[2]-1)
  for (i in 1:(dim(whites)[1])){
    row <- whites[i,1]
    col <- whites[i,2]
    if (is.real.tile(floor, row, col, start.tile)){
      black.neighbours <- black.neighbours(floor, row, col)
      if (black.neighbours == 2){
        floor.new[row, col] <- 1
      }
    }
  }
  floor.new
}

find.useful.range <- function(floor){
  blacks <- which(floor == 1, arr.ind=TRUE, useNames = FALSE)
  border.top.left <- c(min(blacks[,1])-1, min(blacks[,2])-2)
  border.bottom.right <- c(max(blacks[,1])+1, max(blacks[,2])+2)
  c(border.top.left, border.bottom.right)
}

print.floor <- function(floor){
  corners <- find.useful.range(floor)
  row.range <- corners[1]:corners[3]
  col.range <- corners[2]:corners[4]
  floor.lim <- floor[row.range,col.range]
  inactive.rows <- seq(from=1, to=dim(floor.lim)[1]-1, by=2)
  inactive.rows2 <- inactive.rows
  inactive.cols <- seq(from=1, to=dim(floor.lim)[2]-1, by=2)
  inactive.cols2 <- inactive.cols
  if (corners[1] %% 2 == 1){
    inactive.cols <- inactive.cols + 1
    inactive.rows2 <- inactive.rows2 + 1
  }
  else{
    inactive.rows <- inactive.rows + 1
    inactive.cols2 <- inactive.cols2 + 1
  }
  #floor.lim[inactive.rows,inactive.cols] <- 8
  #floor.lim[inactive.rows2,inactive.cols2] <- 8
  dimnames(floor.lim) <- list(row.range,col.range)
  print(floor.lim)
}

day24 <- function(filename)
{
  start.tile <- c(1000,1000)
  floor <- matrix(data=0, nrow=start.tile[1]*2, ncol=start.tile[2]*2)
  f <- read.table(filename, header=FALSE, as.is=TRUE, blank.lines.skip=FALSE)
  for (v in f$V1){
    floor <- flip.tile(floor, v, start.tile)
  }
  #print(sum(floor))
  for (i in 1:100){
    floor <- next.day(floor, start.tile)
    #print.floor(floor)
    cat(i, sum(floor), "\n")
  }
}