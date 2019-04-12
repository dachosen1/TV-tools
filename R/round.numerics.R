# Use with dt[, lapply(X = .SD, FUN = "round.numerics"), .SDcols = ...]
round.numerics <- function(x, digits = 0){
 if(is.numeric(x)){
  x <- round(x = x, digits = digits)
 }
 return(x)
}
