day15 <- function(){
  v <- c(0,6,1,7,2,19,20)
  for (i in (length(v)+1):30000000)
  {
    last <- tail(v, 1)
    h <- head(v, -1)
    occurences <- which(h==last)
    v <- c(v, ifelse(length(occurences) > 0, (i-1)-max(occurences), 0))
    if (i %% 1000) print(i)
  }
  tail(v,1)
}