#MLM tidy table function
tablelmer <- function(x) { # x = test results
  require(broom.mixed)
  require(lmerTest)
  require(stringr)
  require(Hmisc)
  require(numform)
  
  tidyresults <- tidy(x)
  
  #Compute confidence intervals
  confidence_intervals <- confint.merMod(x, method = "Wald")
  confidence_intervals <- as.data.frame(confidence_intervals)
  confidence_intervals$term <- rownames(confidence_intervals)
  
  #Merge confidence intervals
  tidytable <- merge(confidence_intervals, tidyresults, by="term", all.y = TRUE, sort = FALSE)
  
  #Sort
  tidytable <- tidytable[order(tidytable$effect), ]
  tidytable <- tidytable[,c(4,5,1,6,7,2,3,8,9,10)]
  
  #Round
  tidytable[, 4:9] <- tidytable[, 4:9] %>% round(digits=2)
  
  #Tidy p values
  for (i in 1:nrow(tidytable[tidytable$group=="fixed", ])) {
    if(tidytable$p.value[i] < 0.001) {tidytable$p.value[i] <- "< .001"}
    if(tidytable$p.value[i] > 0.001) {tidytable$p.value[i] <- f_num(tidytable$p.value[i], 3)}
  }
  
  return(tidytable) }
