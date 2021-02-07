calc.enc <- function(base, mod, loop){
  x <- base
  for (i in 1:(loop-1)){
    x <- (x * base) %% mod
    loop <- loop + 1
  }
  x
}

calc.loop <- function(base, mod, pub){
  x <- base
  loop <- 1
  while (x != pub){
    x <- (x * base) %% mod
    loop <- loop + 1
  }
  loop
}

day25 <- function(){
  mod <- 20201227
  base <- 7
  # test
  #pub.card <- 5764801
  #pub.door <- 17807724
  # real
  pub.card <- 15335876
  pub.door <- 15086442
  loop.card <- calc.loop(7, mod, pub.card)
  loop.door <- calc.loop(7, mod, pub.door)
  cat("loop card:", loop.card, "door:", loop.door, "\n")
  calc.enc(pub.door, mod, loop.card)
  # or we could have also said:
  # calc.enc(pub.card, mod, loop.door)
  
}