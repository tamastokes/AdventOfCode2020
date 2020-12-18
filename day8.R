day8 <- function(filename, to){
  f <- read.delim(filename, header=FALSE, as.is=TRUE, sep=" ", comment.char = "")
  
  executed <- 0
  accumulator <- 0
  curr_pc <- 1
  backtracked.to <- length(f$V1) + 1 # worst case: infinite loop with each instruction executed exactly once
  while(TRUE){
    acc.increment <- 0
    if (f[curr_pc,]$V1 == "acc"){
      acc.increment <- as.integer(f[curr_pc,]$V2)
      next_pc <- curr_pc + 1
    }
    if (f[curr_pc,]$V1 == "nop"){
        next_pc <- curr_pc + 1
    }
    if (f[curr_pc,]$V1 == "jmp"){
        next_pc <- curr_pc + as.integer(f[curr_pc,]$V2)
    }
    executed <- c(executed, curr_pc)
    accumulator <- c(accumulator, tail(accumulator,1)+acc.increment)
    
    if (next_pc == (length(f$V1)+1)){
      print ("Terminated successfully")
      print (executed)
      print (accumulator)
      return(tail(accumulator,1))
    }
    if (any(executed == next_pc)){
      print ("Infinite loop")
      print (executed)
      print (accumulator)
      print (backtracked.to)
      # part 1
      #return(tail(accumulator,1))
      # part 2
      # backtrack until last modified try (-1)
      executed <- head(executed, backtracked.to-1)
      accumulator <- head(accumulator, backtracked.to-1)
      # backtrack until next nop or jmp 
      while(f[tail(executed,1),]$V1 != "nop" & f[tail(executed,1),]$V1 != "jmp"){
        executed <- executed[-length(executed)]
        accumulator <- accumulator[-length(accumulator)]
      }
      backtracked.to <- length(executed)
      if (backtracked.to < 1){
        print("Problem couldn't be solved")
        return(accumulator)
      }
      if (f[tail(executed,1),]$V1 == "nop"){
        f[tail(executed,1),]$V1 <- "jmp"
      }
      else{
        f[tail(executed,1),]$V1 <- "nop"
      }
      # step back one more so that next iteration executes this
      next_pc <- tail(executed,1)
      executed <- executed[-length(executed)]
      accumulator <- accumulator[-length(accumulator)]
      acc <- tail(accumulator,1)
      print(f)
      print(executed)
      print(accumulator)
      print(acc)
    }
    
    curr_pc <- next_pc
  }
}