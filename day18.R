

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

eval.opposite.precedented <- function(v){
  result <- list()
  result$val <- 0
  result$remaining <- NULL
  if (length(v) == 1){
    result$val <- as.numeric(v)
    result$remaining <- NULL
    return(result)
  }
  if (v[1] == '('){
    result <- eval.opposite.precedented(v[-1])
    return(eval.opposite.precedented(c(result$val, result$remaining)))
  }
  if (v[2] == ')'){
    result$val <- as.numeric(v[1])
    result$remaining <- v[(-1):(-2)]
    return(result)
  }
  if (v[2] == '*'){
    result <- eval.opposite.precedented(v[(-1):(-2)])
    result$val <- as.numeric(v[1]) * result$val
    return(result)
  }
  if (v[2] == '+'){
    if (v[3] == '('){
      result <- eval.opposite.precedented(v[(-1):(-2)])
      result$val <- as.numeric(v[1]) + result$val
      return(result)
    }
    else{
      s <- as.numeric(v[1]) + as.numeric(v[3])
      return(eval.opposite.precedented(c(s,v[(-1):(-3)])))
    }
  }
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

day18.2.wrong <- function(filename){
  f <- read.table(filename, as.is=TRUE, sep="#")
  results <- NULL
  for (l in f$V1){
    s <- strsplit(l, " ")
    ss <- s[[1]]
    sss <- NULL
    for (e in ss){
      obr <- 0
      while (substr(e, obr+1, obr+1) == '('){
        obr <- obr + 1
      }
      cbr <- 0
      while (substr(e, nchar(e)-cbr, nchar(e)-cbr) == ')'){
        cbr <- cbr + 1
      }
      e <- substr(e,1+obr,nchar(e)-cbr)
      sss <- c(sss, rep('(',obr), e, rep(')',cbr))
    }
    result <- eval.opposite.precedented(sss)
    print(result)
    results <- c(results, result$val)
  }
  format(sum(results),scientific = FALSE)
}