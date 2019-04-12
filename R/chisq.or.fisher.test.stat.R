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
