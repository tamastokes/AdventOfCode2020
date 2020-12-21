parse.mask <- function(s){
  v <- NULL
  for (i in 1:nchar(s)){
    ch <- substr(s,i,i)
    if (ch == "1") v <- c(v, 1)
    if (ch == "0") v <- c(v, 0)
    if (ch == "X") v <- c(v, 8)
  }
  v
}

as.bitvector <- function(val, padding=0){
  if (val == 0) return(rep(0,max(1,padding)))
  i = 1
  bitvector <- NULL
  while ((val>0) | (i<=padding)){
    if (val %% 2^i != 0){
      bitvector <- c(1, bitvector)
      val <- val - (2^(i-1))
    }
    else bitvector <- c(0, bitvector)
    i <- i+1
  }
  bitvector
}

bitvector.as.decimal <- function(bitvector){
  val <- 0
  len <- length(bitvector)
  for (i in 1:len){
    if (bitvector[i]) val <- val + (2^(len-i))
  }
  val
}

apply.mask <- function(val.unmasked, mask){
  len <- length(mask)
  val.as.bitvector <- as.bitvector(val.unmasked, len)
  val.unmasked.as.bitvector <- val.as.bitvector
  for (i in 1:len){
    if (mask[i] == 8) next
    if (mask[i] == 0) val.as.bitvector[i] <- 0
    if (mask[i] == 1) val.as.bitvector[i] <- 1
    multiplier <- 2^(len-i)
  }
  masked.val <- bitvector.as.decimal(val.as.bitvector)
  #cat(val.unmasked, "\n", val.unmasked.as.bitvector, "\n", mask, "\n", masked.val, "\n", val.as.bitvector, "\n")
  masked.val
}

apply.floating <- function(bitvector, mask, counter){
  #cat( counter, "  ", bitvector , "\n")
  len <- length(mask)
  if (counter > len) return(bitvector)
  for (i in counter:len){
    if (mask[i] == 8){
      copy1 <- bitvector
      copy2 <- bitvector
      copy1[i] <- 0
      copy2[i] <- 1
      #cat("i: ", i, "\n")
      return( cbind( apply.floating(copy1, mask, i+1), apply.floating(copy2, mask, i+1)))
    }
    multiplier <- 2^(len-i)
  }
  bitvector
}

apply.mask.2 <- function(val.unmasked, mask){
  len <- length(mask)
  val.as.bitvector <- as.bitvector(val.unmasked, len)
  val.unmasked.as.bitvector <- val.as.bitvector
  for (i in 1:len){
    if (mask[i] == 1) val.as.bitvector[i] <- 1
    multiplier <- 2^(len-i)
  }
  m <- apply.floating(val.as.bitvector, mask, 1)
  masked.vals <- apply(m,2,bitvector.as.decimal)
  #cat(val.unmasked, "\n", val.unmasked.as.bitvector, "\n", mask, "\n", masked.val, "\n", val.as.bitvector, "\n")
  masked.vals
}

day14 <- function(filename){
  f <- read.table(filename,as.is=TRUE)
  memory <- NULL
  for (i in 1:length(f$V1)){
    if (f[i,]$V1 == "mask") mask <- parse.mask(f[i,]$V3)
    else{
      address <- substr(f[i,]$V1, 5, nchar(f[i,]$V1)-1)
      val.unmasked <- as.numeric(f[i,]$V3)
      val <- apply.mask(val.unmasked, mask)
      memory[address] <- val
    }
  }
  format(sum(memory),scientific = FALSE)
  
}

day14.2 <- function(filename){
  f <- read.table(filename,as.is=TRUE)
  memory <- list()
  for (i in 1:length(f$V1)){
    if (f[i,]$V1 == "mask") mask <- parse.mask(f[i,]$V3)
    else{
      address <- as.numeric(substr(f[i,]$V1, 5, nchar(f[i,]$V1)-1))
      val.unmasked <- as.numeric(f[i,]$V3)
      addresses <- apply.mask.2(address, mask)
      print(i)
      for (addr in addresses){
        memory[[as.character(addr)]] <- val.unmasked
      }
    }
  }
  format(sum(sapply(memory,function(x) x)),scientific = FALSE)
}