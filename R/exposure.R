# Note:  Patient exposure is computed in relation to their follow-up time.  Those who are lost to follow-up (through events or censoring) will receive an exposure estimate based upon their actual follow-up time if it is less than the overall time.
exposure.old <- function(dat, id.name, time.names, begin.time, end.time, variable.names){
  
  dat <- as.data.frame(dat)
  if(sum(begin.time > end.time)>0){
      return("Error: begin.time must be less than end.time.")
  }
  library(Hmisc)
  if(sum(id.name %nin% names(dat))>0){
      return("Error:  id.name must be in names(dat).")
  }
  if(sum(variable.names %nin% names(dat))>0){
      return("Error: variable.names must all be in names(dat).")
  }
  if(sum(time.names %nin% names(dat))>0){
      return("Error: time.names must be in names(dat).")
  }

  all.ids <- unique(dat[,which(names(dat)==id.name)])
  tt1 <- which(names(dat)==time.names[1])
  tt2 <- which(names(dat)==time.names[2])
  
  if(dat[1,tt1] > dat[1,tt2]){
      tt1 <- tt2
      tt2 <- which(names(dat)==time.names[1])
  }
  
  w <- which(dat[,tt1] < end.time & dat[,tt2] >= begin.time)
  
  dat <- dat[w,]
  
  dat[,tt1] <- pmax(dat[,tt1], begin.time)
  dat[,tt2] <- pmin(dat[,tt2], end.time)
  
  tti <- which(names(dat)==id.name)
  ids <- unique(dat[,tti])
  rates <- matrix(0, nrow=length(all.ids), ncol=length(variable.names))
  follow.times <- sapply(split(dat[,tt2], dat[,tti]), "max")
  for(k in 1:length(variable.names)){
      w <- which(names(dat)==variable.names[k])
      wh <- which(all.ids %in% ids)
      rates[,k] <- sapply(split(dat[,w]*(dat[,tt2]-dat[,tt1])/pmin(end.time-begin.time, follow.times), dat[,tti]),"sum")
  }
  rownames(rates) <- ids
  colnames(rates) <- variable.names
  
  res <- c()
  res$rates <- rates
  res$caption <- sprintf("Rate of exposure between times %.0f and %.0f", begin.time, end.time)
  return(res)
}



calc.exposure <- function(util, begin, end){
  return(sum(util * (end-begin), na.rm = TRUE)/sum(end-begin))
}

# time.names must be a character vector of length 2 with the names of the variables for the beginning and end of the time interval of each row.

# This implementation seems complete as a draft and is ready for testing.
exposure <- function(dat, id.name, time.names, begin.time, end.time, variable.names, append.to.table = TRUE){
  library(data.table)
  
  dat <- as.data.table(dat)
  
  if(sum(begin.time > end.time)>0){
    return("Error: begin.time must be less than end.time.")
  }
  library(Hmisc)
  if(sum(id.name %nin% names(dat))>0){
    return("Error:  id.name must be in names(dat).")
  }
  if(sum(variable.names %nin% names(dat))>0){
    return("Error: variable.names must all be in names(dat).")
  }
  if(sum(time.names %nin% names(dat))>0){
    return("Error: time.names must be in names(dat).")
  }

  tt1 <- time.names[1]
  tt2 <- time.names[2]
  
  if(mean(dat[, get(tt1)] > dat[, get(tt2)], na.rm=TRUE) > 0.5){
    tt1 <- tt2
    tt2 <- time.names[1]
  }
  
  tab <- dat[get(tt1) >= begin.time & get(tt1) < end.time,]

  tab[, new.end := pmin(tab[, get(tt2)], end.time)]
  
  res <- tab[, lapply(.SD, "calc.exposure", begin = get(tt1), end = get("new.end")), .SDcols = variable.names, by = id.name]
  setnames(x = res, old = variable.names, new = sprintf("%s.utilization", variable.names))
  
  if(append.to.table == TRUE){
    newdat <- merge(x = dat, y = res, by = "id", all.x = TRUE)
    return(newdat)
  }
  else{
    return(res)
  }
}