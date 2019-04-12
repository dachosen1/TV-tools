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
