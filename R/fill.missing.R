# Use with dt[, lapply(X = .SD, FUN = "fill.missing"), .SDcols = ...]
fill.missing <- function(x, fill.with){
 if(is.numeric(fill.with)){
  x[is.na(x)] <- fill.with
 }
 if(is.character(fill.with)){
  if(fill.with == "mean"){
   x[is.na(x)] <- mean(x = x, na.rm = TRUE)
  }
  if(fill.with == "median"){
   x[is.na(x)] <- median(x = x, na.rm = TRUE)
  }
 }

 return(x)
}
