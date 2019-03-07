identify.character.coercion.culprits <- function(x){
  w1 <- which(is.na(x))
  
  options(warn = -1)
  y <- as.numeric(x)
  options(warn = 0)
  w2 <- which(is.na(y))
  
  the.indices <- w2[!(w2 %in% w1)]
  the.culprits <- unique(x[the.indices])
  return(the.culprits)
}