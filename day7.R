# The two following methods could be generalized into one..
find.source.nodes.to <- function(m, to){
  v <- which(m[,to]>0)
  for (e in v){
    v <- c(v, find.source.nodes.to(m, e))
  }
  unique(v)
}

sum.weighted.nodes.from <- function(m, from){
  v <- which(m[from,]>0)
  multipliers <- m[from,v]
  total <- 0
  loop <- NULL
  if (length(v) > 0) loop <- 1:length(v)
  for (i in loop){
    sum.desc <- sum.weighted.nodes.from(m, v[i])
    weighted.sum.desc <- multipliers[i] * sum.desc
    total <- total + multipliers[i] + weighted.sum.desc
  }
  total
}

rules.to.matrix <- function(l,w){
  n <- length(l)
  m <- rep(0,n*n)
  dim(m) <- c(n,n)
  for (i in 1:n){
    for (j in 1:(length(l[[i]]))){
      s <- l[[i]][j]
      col <- which(names(l)==s)
      # "other bags." and " " are not real graph nodes, exclude them from processing
      if (length(col) == 1){
        weight <- w[[i]][j]
        m[i,col] = as.integer(weight)
      }
    }
  }
  m
}

day7 <- function(filename, to){
  f <- read.delim(filename, header=FALSE, as.is=TRUE, sep=" ", comment.char = "")
  l <- list()
  w <- list()
  sources <- paste(f[,1],f[,2])
  targets <- paste(f[,6],f[,7])
  weights <- f[,5]
  l[sources] <- targets
  w[sources] <- weights
  loop <- NULL
  if(length(f)>=10) loop <-  seq(from=10,to=length(f),by=4)
  for (i in loop){
    targets <- paste(f[,i],f[,i+1])
    weights <- f[,i-1]
    l2 <- list()
    l2[sources] <- targets
    w2 <- list()
    w2[sources] <- weights
    l <- Map(c, l,l2)
    w <- Map(c, w,w2)
  }
  m <- rules.to.matrix(l,w)
  dimnames(m) <- list(names(l),names(l))
  print(m)
  # part 1
  sources <- find.source.nodes.to(m, which(names(l)==to))
  cat("part 1: ", length(sources), "\n")
  # part 2
  sum.weighted.nodes.from(m, which(names(l)==to))
}
