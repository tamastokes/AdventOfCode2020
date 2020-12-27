

eval.unprecedented <- function(v){
  acc <- NULL
  ops <- 0
  for (e in v){
    if (substr(e,1,1) == '+'){
      ops <- c(ops,1)      
    }
    else if (substr(e,1,1) == '*'){
      ops <- c(ops,2)
    }
    else{
      obr <- 0
      while (substr(e, obr+1, obr+1) == '('){
        ops <- c(ops, 0)
        obr <- obr + 1
      }
      cbr <- 0
      while (substr(e, nchar(e)-cbr, nchar(e)-cbr) == ')'){
        ops <- ops[-max(which(ops==0))]
        cbr <- cbr + 1
      }
      e <- as.numeric(substr(e,1+obr,nchar(e)-cbr))
      # e should now be an ordinary number      
      while (tail(ops,1) > 0){
        top <- acc[length(acc)]
        if (tail(ops,1) == 1){ # '+'
          e <- top + as.numeric(e)
        }
        else{ # '*'
          e <- top * as.numeric(e)
        }
        ops <- head(ops,length(ops)-1)
        acc <- head(acc,length(acc)-1)
      }
      acc <- c(acc, as.numeric(e))
    }
  }
  acc
}

day18 <- function(filename){
  f <- read.table(filename, as.is=TRUE, sep="#")
  results <- NULL
  for (l in f$V1){
    s <- strsplit(l, " ")
    ss <- s[[1]]
    result <- eval.unprecedented(ss)
    print(result)
    results <- c(results, result)
  }
  format(sum(results),scientific = FALSE)
}