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
