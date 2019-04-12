# Use with dt[, lapply(X = .SD, FUN = "mean.missing"), .SDcols = ...]
mean.missing <- function(x){
 return(mean(is.na(x)))
}
