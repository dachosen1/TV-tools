my.summary.values <- function(x){
  if(!is.numeric(x)){
    return("Error:  x is not numeric")
  }
  
  the.min <- min(x, na.rm = TRUE)
  the.max <- max(x, na.rm = TRUE)
  the.quantiles <- quantile(x = x, probs = 1:3 * (0.25), na.rm = TRUE)
  the.mean <- mean(x = x, na.rm = TRUE)
  the.sd <- sd(x = x, na.rm = TRUE)
  total.NA <- sum(is.na(x))
  require(data.table)
  
#  tab <- data.table(Quanity = c("Min", "1st Quartile", "Median", "Mean", "St. Dev", "3rd Quartile", "Max"), )
  
  the.values <- c(the.min, the.quantiles[1:2], the.mean, the.sd, the.quantiles[3], the.max, total.NA)
  names(the.values) <- 
  
  return(the.values)
}


my.summary <- function(dat, SDcols = NA, by = NA, include.total = FALSE){
  require(data.table)
  setDT(dat)
  
  Quantity <- c("Min", "1st Quartile", "Median", "Mean", "St. Dev", "3rd Quartile", "Max", "Total NA")
  
  if(is.na(SDcols[1])){
    SDcols <- 1:ncol(dat)
  }
  if(is.na(by[1])){
    the.results <- data.table(Quantity = Quantity, dat[, lapply(X = .SD, FUN = my.summary.values), .SDcols = SDcols])
  }
  if(!is.na(by[1])){
    the.results <- data.table(Quantity = Quantity, dat[, lapply(X = .SD, FUN = my.summary.values), .SDcols = SDcols, by = by])
    
    if(include.total == TRUE){
      total.results <- data.table(Quantity = Quantity, dat[, lapply(X = .SD, FUN = my.summary.values), .SDcols = SDcols])
      total.results[, (by) := "All Rows"]
      
      the.results <- rbindlist(l = list(the.results, total.results), fill = TRUE)
    }
  }
  
  return(the.results)
}

## Example Calculation
#this.dat <- data.table(x = 1:100, y = 101:200, Group = rep(LETTERS[1:2], 50))

#my.summary(dat = this.dat, SDcols = c("x", "y"), by = "Group", include.total = TRUE)
