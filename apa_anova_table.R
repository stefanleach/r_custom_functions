#ANOVA(afex) tidy table function
apa_anova_table <- function(x) { # x = test results
  require(broom)
  require(afex)
  require(numform)
  
  anovatable <- x$anova_table
  tidytable <- tidy(anovatable)
  
  #Round
  tidytable[, 2:5] <- tidytable[, 2:5] %>% round(digits=2)
  
  #Tidy p values
  for (i in 1:nrow(tidytable)) {
    if(tidytable$p.value[i] < 0.001) {tidytable$p.value[i] <- "< .001"}
    if(tidytable$p.value[i] > 0.001) {tidytable$p.value[i] <- f_num(tidytable$p.value[i], 3)}
  }
  
  return(tidytable) }
