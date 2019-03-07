round.numerics <- function(x, digits){
  if(is.numeric(x)){
    x <- round(x = x, digits = digits)
  }
  return(x)
}

round.and.format.numerics <- function(x, digits, big.mark = ","){
  if(is.numeric(x)){
    x <- prettyNum(x = round(x = x, digits = digits), big.mark = big.mark)
  }
  return(x)
}


mean.numerics <- function(x, na.rm = TRUE){
  if(is.numeric(x)){
    x <- mean(x = x, na.rm = na.rm)
  }
  return(x)
}

sd.numerics <- function(x, na.rm = TRUE){
  if(is.numeric(x)){
    x <- sd(x = x, na.rm = na.rm)
  }
  return(x)
}

median.numerics <- function(x, na.rm = TRUE){
  if(is.numeric(x)){
    x <- median(x = x, na.rm = na.rm)
  }
  return(x)
}

var.numerics <- function(x, na.rm = TRUE){
  if(is.numeric(x)){
    x <- var(x = x, na.rm = na.rm)
  }
  return(x)
}


display.rounded.table <- function(dat, digits, variable.names = NA){
  require(data.table)
  require(DT)
  
  setDT(dat)
  
  if(is.na(variable.names[1])){
    variable.names <- names(dat)
  }
  
  dat.rounded <- dat[, lapply(X = .SD, FUN = "round.numerics", digits = digits), .SDcols = variable.names]
  
  datatable(data = dat.rounded, rownames = FALSE)
}
