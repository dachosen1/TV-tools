my.anova <- function(mod, type, characters.to.remove.from.rn = "`"){
  require(data.table)
  
  if(type == 1){
    mod.anova <- anova(object = mod)
  }
  if(type %in% c(2,3)){
    require(car)
    mod.anova <- Anova(mod = mod, type = type)
  }
  
  num.variables <- length(names(mod$coefficients))
  
  residual.sum.sq <- mod.anova$`Sum Sq`[num.variables + 1]
  
  sum.sq.name <- sprintf("Type %s Sum of Squares", type)
  
  var.names <- gsub(pattern = paste(characters.to.remove.from.rn, collapse = "|"), replacement = "", x = names(mod$coefficients), fixed = TRUE)
  results <- data.table(Variables = c(var.names, "Error"))
  results[, eval(sum.sq.name) := mod.anova$`Sum Sq`]
  results[, df := mod.anova$Df]
  results[, `Mean Square` := get(sum.sq.name) / df]
  results[, F := mod.anova$`F value`]
  results[, `p-value` := mod.anova$`Pr(>F)`]
  results[, `Partial Eta Squared` := get(sum.sq.name) / residual.sum.sq]
  results[.N, `Partial Eta Squared` := NA]
  
  return(results[])
}
