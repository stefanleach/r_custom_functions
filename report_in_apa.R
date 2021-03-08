report_in_apa <- 
 function(x) {
   if(grepl(class(x)[1], "htest")) {
    if(grepl("correlation", x$method)) {
      require(apa)
      require(stringr)
      string <- apa(x, r_ci = TRUE)
      string <- str_replace_all(string, "\\*", "")
      string <- str_replace_all(string, ";", ",")
      return(string)
    }
    if(grepl("t-test", x$method)) {
      require(apa)
      require(stringr)
      string <- apa(x, es = "cohens_d", es_ci = TRUE)
      string <- str_replace_all(string, "\\*", "")
      string <- str_replace_all(string, ";", ",")
      return(string)
    }
   }
   if(grepl(class(x)[1], "afex_aov")) {
      require(apa)
      require(stringr)
      table <- apa(x)
      for (i in 1:nrow(table)) {
       table$text[i] <- str_replace_all(table$text[i], "\\*", "")
       table$text[i] <- str_replace_all(table$text[i], "\\$", "")
       table$text[i] <- str_replace_all(table$text[i], "\\\\", "")
     } 
     return(table)
   }
    if(grepl(class(x)[1], "lmerModLmerTest")) {
      require(lme4)
      require(lmerTest)
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
    if(grepl(class(x)[1], "metacont") | grepl(class(x)[1], "metamean")) {
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
      if(x$pval.fixed > .001) {fixed_p <- paste("p = ", x$pval.fixed %>% f_num(3), sep = "")}
      if(x$pval.fixed < .001) {fixed_p <- "p < .001"}
      
      random_d <- x$TE.random %>% round(2) %>% formatC(digits=2, format='f')
      random_lower <- x$lower.random %>% round(2) %>% formatC(digits=2, format='f')
      random_upper <- x$upper.random %>% round(2) %>% formatC(digits=2, format='f')
      random_z <- x$statistic.random %>% round(2) %>% formatC(digits=2, format='f')
      if(x$pval.random > .001) {random_p <- paste("p = ", x$pval.random %>% f_num(3), sep = "")}
      if(x$pval.random < .001) {random_p <- "p < .001"}
      
      table$string[1] <- paste("d = ", fixed_d, ", 95% CI [", fixed_lower, ", ", fixed_upper, "], Z = ", fixed_z, ", ", fixed_p, sep = "")
      table$string[2] <- paste("d = ", random_d, ", 95% CI [", random_lower, ", ", random_upper, "], Z = ", random_z, ", ", random_p, sep = "")
      
      return(table)
    }
    else {
      stop("I do not support this method :(")
    }
 }
