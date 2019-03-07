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

# Use with dt[, lapply(X = .SD, FUN = "round.numerics"), .SDcols = ...]
round.numerics <- function(x, digits = 0){
  if(is.numeric(x)){
    x <- round(x = x, digits = digits)
  }
  return(x)
}

# Use with dt[, lapply(X = .SD, FUN = "convert.factor.to.character"), .SDcols = ...]
convert.factor.to.character <- function(x){
  if(is.factor(x)){
    x <- as.character(x = x)
  }
  return(x)
}

# Use with dt[, lapply(X = .SD, FUN = "mean.missing"), .SDcols = ...]
mean.missing <- function(x){
  return(mean(is.na(x)))
}

# Use with dt[, lapply(X = .SD, FUN = "sum.missing"), .SDcols = ...]
sum.missing <- function(x){
  return(sum(is.na(x)))
}


## Compute a function on the designated columns split by the designated variables.  Then add an additional row that shows the total results (unsegmented).  Creates a data.table object.

segments.and.totals <- function(dat, function.name, .SDcols, by.name, total.name = "All", ...){
  library(data.table)
  dat <- as.data.table(x = dat)
  
  segmented <- dat[, lapply(X = .SD, FUN = get(function.name), ... = ...), .SDcols = .SDcols, by = by.name]
  total <- dat[, lapply(X = .SD, FUN = get(function.name), ... = ...), .SDcols = .SDcols]
  total[, eval(by.name) := total.name]
  
  res <- rbindlist(l = list(segmented, total), fill = TRUE)
  return(res)
}


# data.table equivalent to plyr::mapvalues
dt_mapvalues <- function(dat, variable.name, from, to){
  library(data.table)
  dat <- as.data.table(dat)
  
  for(i in 1:length(from)){
    set(x = dat, i = dat[get(variable.name) == from[i], .I], j = variable.name, value = to[i])
  }

  return(dat)  
}

