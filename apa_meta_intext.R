#Meta-analysis
#print fixed effects table in APA-style for reporting in-text
#packages: meta, numform, tidyverse
apa_meta_intext <- function(x) {
  require(meta)
  require(numform)
  require(tidyverse)
  table <- 
    data.frame(effect = c("fixed", "random"),
               string = NA)
  
  fixed_d <- x$TE.fixed %>% round(2) %>% formatC(digits=2, format='f')
  fixed_lower <- x$lower.fixed %>% round(2) %>% formatC(digits=2, format='f')
  fixed_upper <- x$upper.fixed %>% round(2) %>% formatC(digits=2, format='f')
  fixed_z <- x$statistic.fixed %>% round(2) %>% formatC(digits=2, format='f')
  if(x$pval.fixed > .001)  {fixed_p <- paste("p = ", x$pval.fixed %>% f_num(3), sep = "")}
  if(x$pval.fixed < .001)  {fixed_p <- "p < .001"}
  
  random_d <- x$TE.random %>% round(2) %>% formatC(digits=2, format='f')
  random_lower <- x$lower.random %>% round(2) %>% formatC(digits=2, format='f')
  random_upper <- x$upper.random %>% round(2) %>% formatC(digits=2, format='f')
  random_z <- x$statistic.random %>% round(2) %>% formatC(digits=2, format='f')
  if(x$pval.random > .001)  {random_p <- paste("p = ", x$pval.random %>% f_num(3), sep = "")}
  if(x$pval.random < .001)  {random_p <- "p < .001"}
  
  table$string[1] <- paste("d = ", fixed_d, ", 95% CI [", fixed_lower, ", ", fixed_upper, "], Z = ", fixed_z, ", ", fixed_p, sep = "")
  table$string[2] <- paste("d = ", random_d, ", 95% CI [", random_lower, ", ", random_upper, "], Z = ", random_z, ", ", random_p, sep = "")
  
  return(table)
}
