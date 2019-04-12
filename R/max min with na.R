max.with.na <- function(x){
  y <- x[!is.na(x)]
  if(length(y) == 0){
    return(NA_real_)
  }
  if(length(y) > 0){
    return(as.numeric(x = max(y, na.rm = TRUE)))
  }
}

min.with.na <- function(x){
  y <- x[!is.na(x)]
  if(length(y) == 0){
    return(NA_real_)
  }
  if(length(y) > 0){
    return(as.numeric(x = min(y, na.rm = TRUE)))
  }
}