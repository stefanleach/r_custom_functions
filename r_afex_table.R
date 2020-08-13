#ANOVA(afex) tidy table function
tableafex <- function(x) { # x = test results
  require(broom)
  require(afex)
  require(numform)
  
  anovatable <- x$anova_table
  tidytable <- tidy(anovatable)
  
  #Round
  tidytable[, 2:5] <- tidytable[, 2:5] %>% round(digits=2)
  tidytable[, 6] <- tidytable[, 6] %>% round(digits=3)
  
  return(tidytable) }