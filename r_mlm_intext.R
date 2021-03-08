#Mixed effect models
#print fixed effects table in APA-style for reporting in-text
printlmer <- function(x) {
  require(broom)
  require(broom.mixed)
  require(numform)
  require(tidyverse)
  
  results <- 
    x %>%
    tidy()
  
  table <- 
    x %>%
    tidy() %>%
    mutate(result = NA) %>%
    filter(effect=="fixed") %>%
    dplyr::select(term, result)
  
  CIs <- x %>% confint.merMod(method = "Wald") %>% as.data.frame %>% mutate(term = rownames(.))
  
  for (i in table$term) {
    estimate <- results %>% filter(term==i) %>% pull(estimate) %>% round (2) %>% formatC(digits=2, format='f')
    se <- results %>% filter(term==i) %>% pull(std.error) %>% round (2) %>% formatC(digits=2, format='f')
    CI_low <- CIs %>% filter(term==i) %>% pull(`2.5 %`) %>% round (2) %>% formatC(digits=2, format='f')
    CI_high <- CIs %>% filter(term==i) %>% pull(`97.5 %`) %>% round (2) %>% formatC(digits=2, format='f')
    if(results$p.value[results$term==i] > .001)  {p <- paste("p = ", results %>% filter(term==i) %>% pull(p.value) %>% f_num(3), sep ="")}
    if(results$p.value[results$term==i] < .001)  {p <- "p < .001"}
    string <- paste("coef = ", estimate, ", SE = ", se, ", 95% CI [", CI_low, ", ", CI_high, "], ", p, sep = "")
    table$result[table$term==i] <- string
  }
  return(table)
}
