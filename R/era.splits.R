# Split data set rows overlapping different time eras into separate records within each era.  Newly created records are appended in additional rows to the data set.  If a row encapsulates 0-2 years, and we wish to split it into the first and second years, the original row will be converted to a time interval of 0-1, and a row for 1-2 will be appended to the end of the data set.
era.splits.old <- function(dat, cut.points, time.names=c("t1", "t2")){
  library(Hmisc)
  
  if(sum(time.names %nin% names(dat))>0){
    return("Error: time.names must all be in names(dat).")
  }
  
  tt1 <- which(names(dat)==time.names[1])
  tt2 <- which(names(dat)==time.names[2])
  
  cut.points <- cut.points[cut.points <= max(dat[,tt2]) & cut.points >= 0]
  cut.points <- sort(unique(cut.points))
  
  for(cutoff in cut.points){
    w <- which(dat[,tt1] < cutoff & dat[,tt2] > cutoff)
    if(length(w)>0){
      dd <- dat[w,]
      dd[,tt1] <- cutoff
      dat[w,tt2] <- cutoff
      
      dat <- rbind(dat, dd)
    }
  }
  
  return(dat)
}

# This implementation is ready for testing.
era.splits <- function(dat, cut.points, time.names=c("t1", "t2")){
  library(Hmisc)
  library(data.table)
  
  dat <- as.data.table(dat)
  
  if(sum(time.names %nin% names(dat))>0){
    return("Error: time.names must all be in names(dat).")
  }
  
  if(!is.character(time.names)){
    return("Error:  time.names mut be a character vector.")
  }
  
  if(length(time.names) != 2){
    return("Error:  time.names must be a character vector of length 2")
  }
  
  tt1 <- time.names[1]
  tt2 <- time.names[2]
  
  if(mean(dat[, get(tt1)] > dat[, get(tt2)], na.rm=TRUE) > 0.5){
    tt1 <- tt2
    tt2 <- time.names[1]
  }
  
  cut.points <- cut.points[cut.points <= max(dat[,get(tt2)]) & cut.points >= 0]
  cut.points <- sort(unique(cut.points))
  
  for(cutoff in cut.points){
    w <- which(dat[,get(tt1)] < cutoff & dat[,get(tt2)] > cutoff)
    
    if(length(w)>0){
      dd <- dat[w,]
      dd[, tt1] <- cutoff
      dat[w, tt2] <- cutoff
      
      dat <- rbind(dat, dd)
    }
  }
  
  return(dat)
}
