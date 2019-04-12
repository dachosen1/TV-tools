
## Counts including a mix of measured and unmeasured segments.  Unmeasured segments are added as additional rows with zero counts.  Note:  Needs to be extended to multiple variables in the by.names.
category.counts <- function(dat, by.names, count.name = NA, additional.segments = NA){
  library(data.table)
  dat <- setDT(x = dat)
  
  measured.counts <- dat[, .N, by = by.names]
  if(is.na(count.name)){
    count.name <- "N"
  }
  else{
    setnames(x = measured.counts, old = "N", new = count.name)
  }
  other.values <- additional.segments[!(additional.segments %in% dat[, unique(by.names)])]
  
  if(!is.na(additional.segments[1])){
    unmeasured.counts <- data.table(other.values, 0)
    setnames(x = unmeasured.counts, old = names(unmeasured.counts), new = c(by.names, count.name))
    all.counts <- rbindlist(l = list(measured.counts, unmeasured.counts), fill = TRUE)
  }
  else{
    all.counts <- measured.counts
  }
  
  return(all.counts)
}
