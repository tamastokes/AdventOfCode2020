day6 <- function(filename){
  f <- read.delim(filename,blank.lines.skip=FALSE, header=FALSE, as.is=TRUE, comment.char = "")
  result <- NULL
  # part 1
  v <- NULL
  # part 2
  v <- c("a","b","c","d","e","f","g","h","i","j",
         "k","l","m","n","o","p","q","r","s","t",
         "u","v","w","x","y","z")
  for (l in f$V1){
    if (nchar(l) == 0){
      result <- c(result, length(v))
      # part 1
      v <- NULL
      # part 2
      v <- c("a","b","c","d","e","f","g","h","i","j",
             "k","l","m","n","o","p","q","r","s","t",
             "u","v","w","x","y","z")
    }
    else{
      # part 1
      #v <- union(v, substring(l,1:nchar(l),1:nchar(l)))
      # part 2
      v <- intersect(v, substring(l,1:nchar(l),1:nchar(l)))
    }
  }
  result <- c(result, length(v))
  sum(result)
}
