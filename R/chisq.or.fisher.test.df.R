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
