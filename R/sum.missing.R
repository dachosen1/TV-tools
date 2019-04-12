# Use with dt[, lapply(X = .SD, FUN = "sum.missing"), .SDcols = ...]
sum.missing <- function(x){
 return(sum(is.na(x)))
}
