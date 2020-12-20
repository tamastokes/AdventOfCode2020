turn <- function(curr.direction, left.or.right, degree){
  directions <- c('N','E','S','W')
  v <- rep(directions,100)
  pos <- 200 + which(directions==curr.direction) + (degree/90) * ifelse(left.or.right=="L",-1,1)
  v[pos]
}

day12 <- function(filename){
  f <- read.table(filename, as.is = TRUE)
  directions <- c('N','E','S','W')
  instructions <- NULL
  quantities <- NULL
  direction <- 'E'
  for (e in f$V1){
    instr <- substr(e,1,1)
    val <- as.integer(substr(e,2,nchar(e)))
    if (any(instr==directions)){
      instructions <- c(instructions, instr)
      quantities <- c(quantities, val)
    }
    else{
      if (instr=='F'){
        instructions <- c(instructions, direction)
        quantities <- c(quantities, val)
      }
      if (any(instr=='R',instr=='L')){
        direction <- turn(direction, instr, val)
        instructions <- c(instructions, direction)
        quantities <- c(quantities, 0)
      }
    }
  }
  groupped <- tapply(quantities,instructions,sum)
  abs(groupped['N']-groupped['S']) + abs(groupped['E']-groupped['W'])
}