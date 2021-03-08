#Meta-analysis
#print fixed effects table in APA-style for reporting in-text
#packages: meta, numform, tidyverse
apa_meta_table <- function(x) {
  require(meta)
  require(numform)
  require(tidyverse)
  table <- 
    data.frame(effect = c("fixed", "random"),
               string = NA)
  
  fixed_d <- temp$TE.fixed %>% round(2) %>% formatC(digits=2, format='f')
  fixed_lower <- temp$lower.fixed %>% round(2) %>% formatC(digits=2, format='f')
  fixed_upper <- temp$upper.fixed %>% round(2) %>% formatC(digits=2, format='f')
  fixed_z <- temp$statistic.fixed %>% round(2) %>% formatC(digits=2, format='f')
  if(temp$pval.fixed > .001)  {fixed_p <- paste("p = ", temp$pval.fixed %>% f_num(3), sep = "")}
  if(temp$pval.fixed < .001)  {fixed_p <- "p < .001"}
  
  random_d <- temp$TE.random %>% round(2) %>% formatC(digits=2, format='f')
  random_lower <- temp$lower.random %>% round(2) %>% formatC(digits=2, format='f')
  random_upper <- temp$upper.random %>% round(2) %>% formatC(digits=2, format='f')
  random_z <- temp$statistic.random %>% round(2) %>% formatC(digits=2, format='f')
  if(temp$pval.random > .001)  {random_p <- paste("p = ", temp$pval.random %>% f_num(3), sep = "")}
  if(temp$pval.random < .001)  {random_p <- "p < .001"}
  
  table$string[1] <- paste("d = ", fixed_d, ", 95% CI [", fixed_lower, ", ", fixed_upper, "], Z = ", fixed_z, ", ", fixed_p, sep = "")
  table$string[2] <- paste("d = ", random_d, ", 95% CI [", random_lower, ", ", random_upper, "], Z = ", random_z, ", ", random_p, sep = "")
  
  return(table)
}