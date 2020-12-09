check.if.valid <- function(l){
  print(l)
  ifelse( (length(l) == 8 | (length(l) == 7 & is.null(l$cid))), TRUE, FALSE)
}

parseInput <- function(filename){
  f <- read.delim(filename,blank.lines.skip=FALSE, header=FALSE, as.is=TRUE, comment.char = "")
  v <- NULL
  m <- list()
  n <- NULL
  for (l in f$V1){
    if (nchar(l) == 0){
      n <- c(n, check.if.valid(m))
      m <- list()
    }
    else{
      s <- strsplit(l," ")
      ss <- sapply(s[[1]],strsplit, ":")
      for( sss in ss){
        m[[sss[1]]] <- sss[2]
      }
    }
  }
  n <- c(n, check.if.valid(m))
  sum(n)
}