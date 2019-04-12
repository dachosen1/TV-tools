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
