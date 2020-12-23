in.range <- function(n, r){
  return(n>=r[1] & n<=r[2])
}

check.validity.for.single.rule <- function(rule, v){
  in.range(v, rule[1:2]) | in.range(v, rule[3:4])
}

check.validity <- function(v, rules){
  valid <- apply(rules, 2, check.validity.for.single.rule, v)
  apply(valid,1,sum)
}

check.validity.2 <- function(v, rules){
  valid <- apply(rules, 2, check.validity.for.single.rule, v)
  apply(valid,2,prod)
}


day16 <- function(filename){
  con <- file(filename)
  lines <- readLines(con)
  state <- 1
  rulesl <- list()
  rulesm <- NULL
  nearby.tickets <- NULL
  for (l in lines){
    if (l == "") next
    if (l == "your ticket:"){
      state <- 2
      next
    }
    if (l == "nearby tickets:"){
      state <- 3
      next
    }
    if (state == 1){
      s <- strsplit(l, " ")
      category <- substr(s[[1]][1],1,nchar(s[[1]][1])-1)
      range1 <- s[[1]][2]
      r1 <- strsplit(range1, "-")
      range2 <- s[[1]][4]
      r2 <- strsplit(range2, "-")
      rulesl[[category]] <- as.integer(c(r1[[1]],r2[[1]]))
      if (is.null(rulesm)){
        rulesm <- as.integer(c(r1[[1]],r2[[1]]))
      }
      else{
        rulesm <- cbind(rulesm,as.integer(c(r1[[1]],r2[[1]])))
      }
    }
    if (state == 2){
      s <- strsplit(l, ",")
      my.ticket <- as.integer(s[[1]])
      state <- 3
      next()
    }
    if (state == 3){
      s <- strsplit(l, ",")
      a.nearby.ticket <- as.integer(s[[1]])
      if(is.null(nearby.tickets)){
        nearby.tickets <- a.nearby.ticket
      }
      else{
        nearby.tickets <- cbind(nearby.tickets, a.nearby.ticket)
      }
    }
  }
  dimnames(nearby.tickets) <- NULL
  dimnames(rulesm) <- NULL
  valid <- apply(nearby.tickets,2,check.validity,rulesm)
  # part 1
  sum(nearby.tickets[valid==0])
  # part 2
  valid.tickets <- nearby.tickets[,apply(valid,2,function(x) min(x)>0)]
  category.by.position <- apply(valid.tickets,1,check.validity.2,rulesm)
  cat.pos <- list()

  while(sum(category.by.position) > 0){
    #print(category.by.position)
    s <- apply(category.by.position,2,sum)
    position <- which(s==1)
    category <- which(category.by.position[,position] == 1)
    #cat(category, position, "\n")
    cat.pos[[names(rulesl)[category]]] <- position
    category.by.position[category,] <- 0
  }  
  interesting.categories <- names(rulesl)[substr(names(rulesl),1,9) == "departure"]
  prod(sapply( cat.pos[interesting.categories], function(x,my.numbers) { my.numbers[x] }, my.ticket))
}