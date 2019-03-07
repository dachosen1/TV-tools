cross.sectional.data.old <- function(dat, id.name, time.names, outcome.names=NA, time=0){
  library(Hmisc)
  if(sum(outcome.names %nin% names(dat))>0){
    return("Error: outcome.names must all be in names(dat).")
  }
  if(sum(time.names %nin% names(dat))>0){
    return("Error: time.names must all be in names(dat).")
  }
  tt1 <- which(names(dat)==time.names[1])
  tt2 <- which(names(dat)==time.names[2])
  
  if(dat[1,tt1] > dat[1,tt2]){
    tt1 <- tt2
    tt2 <- which(names(dat)==time.names[1])
  }
  
  w <- which(dat[,tt1]<=time & dat[,tt2] >= time)
  
  baseline <- dat[w,]
  ti <- which(names(baseline)==id.name)
  o <- order(baseline[,ti])
  baseline <- baseline[o,]
  
  if(is.na(outcome.names[1])){
    return(baseline)
  }
  
  source("first.event.R")
  
  event.time <- first.event(dat, id.name, outcome.names, time.names[1])$tab
  o <- order(as.numeric(rownames(event.time)))
  event.time <- event.time[o,] - time
  
  for(k in 1:ncol(event.time)){
    baseline <- cbind(baseline, as.numeric(event.time[,k]))
    names(baseline)[ncol(baseline)] <- sprintf("%s.time", colnames(event.time)[k])
  }
  return(baseline)
}

# The updated draft is complete and ready for testing.
cross.sectional.data <- function(dat, id.name, time.names, outcome.names, time){
  library(data.table)
  library(Hmisc)
  if(!is.na(outcome.names[1])){
    if(sum(outcome.names %nin% names(dat))>0){
      return("Error: outcome.names must all be in names(dat).")
    }
  }
  if(sum(time.names %nin% names(dat))>0){
    return("Error: time.names must all be in names(dat).")
  }
  
  dat <- as.data.table(dat)
  
  tt1 <- time.names[1]
  tt2 <- time.names[2]
  
  if(mean(dat[, get(tt1)] > dat[, get(tt2)], na.rm=TRUE) > 0.5){
    tt1 <- tt2
    tt2 <- time.names[1]
  }
  
  w <- which(dat[,get(tt1)] <= time & dat[,get(tt2)] >= time)
  
  baseline <- dat[w,]
  
  setorderv(x = baseline, cols = id.name)

  if(is.na(outcome.names[1])){
    return(baseline)
  }
  
  source("first.event.R")
  
  first.ones <- first.event(dat = dat, id.name = id.name, outcome.names = outcome.names, time.name = time.names[1], append.to.table = FALSE) 
  
  event.time <- first.ones
  
  names(event.time)[names(event.time != id.name)] <- sprintf("%s.time", names(event.time)[names(event.time != id.name)])
  
  source("followup.time.R")
  
  folls <- followup.time(dat = dat, id.name = id.name, time.name = tt2, append.to.data = FALSE)
  
  folls$follow.up.time <- folls$follow.up.time - time
  
  event.time <- merge(x = event.time, y = folls, by = id.name)
  
  
  
  res <- merge(x = baseline, y = event.time, by = id.name, all.x = TRUE)
  
  res$cross.sectional.time <- time
  
  return(res)
}