number.measured <- function(x){
  return(sum(!is.na(x)))
}

t.test.p.value <- function(x, splitter, min.measured = 2){
  values <- split(x = x, f = splitter)
  
  nvalues <- as.numeric(lapply(X = values, FUN = "number.measured"))
  
  if(min(nvalues) < min.measured){
    return(NA)
  }
  if(min(nvalues) >= min.measured){
    res <- t.test(x = values[[1]], y = values[[2]])
    pval <- round(res$p.value, 3)
    return(pval)
  }
}

t.test.stat <- function(x, splitter, min.measured = 2){
  values <- split(x = x, f = splitter)
  
  nvalues <- as.numeric(lapply(X = values, FUN = "number.measured"))
  
  if(min(nvalues) < min.measured){
    return(NA)
  }
  
  if(min(nvalues) >= min.measured){
    res <- t.test(x = values[[1]], y = values[[2]])
    tval <- round(res$statistic, 2)
    return(tval)
  }
}

t.test.df <- function(x, splitter, min.measured = 2){
  values <- split(x = x, f = splitter)

  nvalues <- as.numeric(lapply(X = values, FUN = "number.measured"))
  
  if(min(nvalues) < min.measured){
    return(NA)
  }
  
  if(min(nvalues) >= min.measured){
    res <- t.test(x = values[[1]], y = values[[2]])
    pval <- round(res$parameter, 0)
    return(pval)
  }
}

round.sd <- function(x, digits = 1){
  y <- round(x = sd(x, na.rm = TRUE), digits = digits)
  return(y)
}

se <- function(x, na.rm = TRUE){
  return(sd(x, na.rm = na.rm)/sqrt(sum(!is.na(x))))
}

round.se <- function(x, digits = 1){
  y <- round(x = se(x), digits = digits)
  return(y)
}

chisq.or.fisher.test.stat <- function(x, splitter){
  values <- table(x, splitter)
  
  if(min(values) <= 8){
    res <- fisher.test(values)
    res$statistic <- NA
  }
  else{
    res <- chisq.test(values)
  }
  return(res$statistic)
}


chisq.or.fisher.test.df <- function(x, splitter){
  values <- table(x, splitter)
  
  if(min(values) <= 8){
    res <- fisher.test(values)
    res$parameter <- NA
  }
  else{
    res <- chisq.test(values)
  }
  return(res$parameter)
}

chisq.or.fisher.test.p.value <- function(x, splitter){
  values <- table(x, splitter)
  
  if(min(values) <= 8){
    res <- fisher.test(values)
  }
  else{
    res <- chisq.test(values)
  }
  pval <- round(res$p.value, 3)
  return(pval)
}

comparison.test.p.value <- function(x, splitter){
  unique.values <- unique(x)
  unique.values <- unique(x[!is.na(x)])
  
  if(length(unique.values) == 2){
    if(mean(sort(unique.values) == c(0,1)) == 1){
      
      return(chisq.or.fisher.test.p.value(x = x, splitter = splitter))
    }
    else{
      return(t.test.p.value(x = x, splitter = splitter))
    }
  }
  else{
    return(t.test.p.value(x = x, splitter = splitter))
  }
}

comparison.test.statistic <- function(x, splitter){
  unique.values <- unique(x)
  unique.values <- unique(x[!is.na(x)])
  
  if(length(unique.values) == 2){
    if(mean(sort(unique.values) == c(0,1)) == 1){
      
      return(chisq.or.fisher.test.stat(x = x, splitter = splitter))
    }
    else{
      return(t.test.stat(x = x, splitter = splitter))
    }
  }
  else{
    return(t.test.stat(x = x, splitter = splitter))
  }
}


comparison.test.df <- function(x, splitter){
  unique.values <- unique(x)
  unique.values <- unique(x[!is.na(x)])
  
  if(length(unique.values) == 2){
    if(mean(sort(unique.values) == c(0,1)) == 1){
      
      return(chisq.or.fisher.test.df(x = x, splitter = splitter))
    }
    else{
      return(t.test.df(x = x, splitter = splitter))
    }
  }
  else{
    return(t.test.df(x = x, splitter = splitter))
  }
}

comparison.test.names <- function(x, splitter){
  unique.values <- unique(x)
  unique.values <- unique(x[!is.na(x)])
  
  if(length(unique.values) == 2){
    if(mean(sort(unique.values) == c(0,1)) == 1){
      
      return("Chi Square/Fisher Test")
    }
    else{
      return("t Test")
    }
  }
  else{
    return("t Test")
  }
}

get.sample.size <- function(x){
  return(sum(!is.na(x)))
}

round.mean <- function(x, digits = 1){
  y <- round(x = mean(x, na.rm = TRUE), digits = digits)
  return(y)
}


mean.segments.and.totals <- function(dat, SDcols, by, digits = 1){
  library(data.table)
  
  sd.name <- "SD"
  se.name <- "SE"
  n.segmented <- dat[, lapply(X = .SD, FUN = "get.sample.size"), .SDcols = SDcols, by = by]
  n.segmented[, eval(by) := sprintf("%s - n", get(by))]
  mean.segmented <- dat[, lapply(X = .SD, FUN = "round.mean", digits = digits), .SDcols = SDcols, by = by]
  mean.segmented[, eval(by) := sprintf("%s - Mean", get(by))]
  sd.segmented <- dat[, lapply(X = .SD, FUN = "round.sd", digits = digits), .SDcols = SDcols, by = by]
  sd.segmented[, eval(by) := sprintf("%s - %s", get(by), sd.name)]
  
  se.segmented <- dat[, lapply(X = .SD, FUN = "round.se", digits = digits), .SDcols = SDcols, by = by]
  se.segmented[, eval(by) := sprintf("%s - %s", get(by), se.name)]
  
  tvalues <- dat[, lapply(X = .SD, FUN = "comparison.test.statistic", splitter = get(by)), .SDcols = SDcols]
  tvalues[, eval(by) := "test stat"]
  
  dfvalues <- dat[, lapply(X = .SD, FUN = "comparison.test.df", splitter = get(by)), .SDcols = SDcols]
  dfvalues[, eval(by) := "df"]
  
  pvalues <- dat[, lapply(X = .SD, FUN = "comparison.test.p.value", splitter = get(by)), .SDcols = SDcols]
  pvalues[, eval(by) := "p"]
  n.all <- dat[, lapply(X = .SD, FUN = "get.sample.size"), .SDcols = SDcols]
  n.all[, eval(by) := "All - n"]
  mean.all <- dat[, lapply(X = .SD, FUN = "round.mean", digits = digits), .SDcols = SDcols]
  mean.all[, eval(by) := "All - Mean"]
  se.all <- dat[, lapply(X = .SD, FUN = "round.se", digits = digits), .SDcols = SDcols]
  se.all[, eval(by) := sprintf("All - %s", se.name)]
  
  sd.all <- dat[, lapply(X = .SD, FUN = "round.sd", digits = digits), .SDcols = SDcols]
  sd.all[, eval(by) := sprintf("All - %s", sd.name)]
  
  all.rows <- rbindlist(l = list(n.segmented, mean.segmented, sd.segmented, se.segmented, tvalues, dfvalues, pvalues, n.all, mean.all, sd.all, se.all), fill = TRUE)
  by.values <- all.rows[, get(by)]
  
  results <- all.rows[, data.table(t(.SD), keep.rownames=TRUE), .SDcols=-by]
  setnames(x = results, old = names(results), new = c("Trait", by.values))
  
  test.names <- as.data.table(t(dat[, lapply(X = .SD, FUN = "comparison.test.names", splitter = get(by)), .SDcols = SDcols]), keep.rownames = TRUE)
  setnames(x = test.names, old = names(test.names), new = c("Trait", "Test"))
  
  results <- merge(x = results, y = test.names, by = "Trait")
  
  setcolorder(x = results, neworder = c("Trait", "Test", names(results)[names(results) %nin% c("Trait", "Test")]))
  
  return(results)
}
