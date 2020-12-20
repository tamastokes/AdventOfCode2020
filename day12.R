turn <- function(curr.direction, left.or.right, degree){
  directions <- c('N','E','S','W')
  v <- rep(directions,100)
  pos <- 200 + which(directions==curr.direction) + (degree/90) * ifelse(left.or.right=="L",-1,1)
  v[pos]
}

rotate.waypoint <- function(curr.position, left.or.right, degree){
  coord.to.negate <- ifelse(left.or.right=='L', 1, 2)
  for (i in 1:(degree/90)){
    curr.position <- rev(curr.position)
    curr.position[coord.to.negate] <- (-1)*curr.position[coord.to.negate]
  }
  curr.position
}

day12 <- function(filename, part=1){
  f <- read.table(filename, as.is = TRUE)
  directions <- c('E','N','S','W')
  instructions <- directions
  quantities <- c(10, 1, 0, 0) # initial values coming from the problem-definition
  spaceship.pos <- c(0,0)
  for (e in f$V1){
    instr <- substr(e,1,1)
    val <- as.integer(substr(e,2,nchar(e)))
    if (any(instr==directions)){
      instructions <- c(instructions, instr)
      quantities <- c(quantities, val)
    }
    else{
      if (instr=='F'){
        groupped <- tapply(quantities,instructions,sum)
        total.north <- groupped['N']-groupped['S']
        total.east <- groupped['E']-groupped['W']
        spaceship.pos <- spaceship.pos + val*c(total.north,total.east)
        cat("F, pos: ", spaceship.pos, "\n")
        print(groupped)
      }
      if (any(instr=='R',instr=='L')){
        groupped <- tapply(quantities,instructions,sum)
        total.north <- groupped['N']-groupped['S']
        total.east <- groupped['E']-groupped['W']
        pos <- c(total.east, total.north)
        cat("RL pos: ", pos, "\n")
        new.position <- rotate.waypoint(pos,instr,val)
        cat(" new pos: ", new.position, "\n")
        instructions <- directions
        quantities <- c(new.position, 0, 0)
        cat(" new instr: ", instructions, "\n")
        cat(" new quant: ", quantities, "\n")
      }
    }
  }
  sum(abs(spaceship.pos))
}