#MLM tidy table function
tablelmer <- function(x) { # x = test results
  require(papaja)
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
  tidytable <- merge(confidence_intervals, tidyresults, by="term", all.y = TRUE)
  
  #Sort
  tidytable <- tidytable[order(tidytable$effect), ]
  tidytable <- tidytable[,c(4,5,1,6,7,2,3,8,9,10)]
  
  #Round to 2 digits
  tidytable[, 4:9] <- tidytable[, 4:9] %>% round(digits=2)
  
  return(tidytable) }
