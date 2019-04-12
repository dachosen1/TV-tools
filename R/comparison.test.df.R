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
