check.if.valid <- function(l,i){
  #print(l)
  if (!(( length(l) == 8 & !is.null(l$cid)) | (length(l) == 7 & is.null(l$cid)))) return(FALSE)
  if (!(all(is.element(substring(l$byr,1:4,1:4) ,c("1","2","3","4","5","6","7","8","9","0"))))) return(FALSE)
  if (l$byr < 1920 | l$byr > 2002) return(FALSE)
  if (!(all(is.element(substring(l$iyr,1:4,1:4) ,c("1","2","3","4","5","6","7","8","9","0"))))) return(FALSE)
  if (l$iyr < 2010 | l$iyr > 2020) return(FALSE)
  if (!(all(is.element(substring(l$eyr,1:4,1:4) ,c("1","2","3","4","5","6","7","8","9","0"))))) return(FALSE)
  if (l$eyr < 2020 | l$eyr > 2030) return(FALSE)
  # hgt
  cm.or.in <- substr(l$hgt,nchar(l$hgt)-1,nchar(l$hgt))
  if (cm.or.in != "cm" & cm.or.in != "in") return(FALSE)
  val <- substr(l$hgt,1,nchar(l$hgt)-2)
  if (cm.or.in == "cm" & (val < 150 | val > 193)) return(FALSE)
  if (cm.or.in == "in" & (val < 59 | val > 76)) return(FALSE)
  #hcl
  if (nchar(l$hcl) != 7) return(FALSE)
  if (substr(l$hcl,1,1) != "#") return(FALSE)
  if (!(all(is.element(substring(l$hcl,2:6,2:6) ,c("1","2","3","4","5","6","7","8","9","0","a","b","c","d","e","f"))))) return(FALSE)
  #ecl
  if (!is.element(l$ecl, c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"))) return(FALSE)
  #pid
  if (nchar(l$pid) != 9) return(FALSE)
  if (!(all(is.element(substring(l$pid,1:9,1:9) ,c("1","2","3","4","5","6","7","8","9","0"))))) return(FALSE)
  print(i)
  print(l)
  TRUE
}

parseInput <- function(filename){
  f <- read.delim(filename,blank.lines.skip=FALSE, header=FALSE, as.is=TRUE, comment.char = "")
  v <- NULL
  m <- list()
  n <- NULL
  i <- 1
  for (l in f$V1){
    if (nchar(l) == 0){
      n <- c(n, check.if.valid(m,i))
      i <- i + 1
      m <- list()
    }
    else{
      s <- strsplit(l," ")
      ss <- sapply(s[[1]],strsplit, ":")
      for( sss in ss){
        if (nchar(sss[1]) == 3) m[[sss[1]]] <- sss[2]
      }
    }
  }
  n <- c(n, check.if.valid(m,i))
  sum(n)
}


