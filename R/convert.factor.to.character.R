# Use with dt[, lapply(X = .SD, FUN = "convert.factor.to.character"), .SDcols = ...]
convert.factor.to.character <- function(x){
 if(is.factor(x)){
  x <- as.character(x = x)
 }
 return(x)
}
