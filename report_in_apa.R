z2cor <- 
  function(x) {
    res <- (exp(2 * x) - 1) / (exp(2 * x) + 1)
    res
}
report_in_apa <- 
 function(x) {
   if(grepl("Pearson's product-moment correlation", x$method)) {
     r_rounded <- sprintf("%.2f", round(x$estimate[[1]], 2))
     r_no_leading <- gsub("0\\.", ".", as.character(r_rounded))
     r = r_no_leading
     r_lci_rounded <- sprintf("%.2f", round(x$conf.int[1], 2))
     r_lci_no_leading <- gsub("0\\.", ".", as.character(r_lci_rounded))
     r_lci = r_lci_no_leading
     r_uci_rounded <- sprintf("%.2f", round(x$conf.int[2], 2))
     r_uci_no_leading <- gsub("0\\.", ".", as.character(r_uci_rounded))
     r_uci = r_uci_no_leading
     t <- sprintf("%.2f", round(x$statistic[[1]], 2))
     df <- x$parameter[[1]]
     
     if(x$p.value > .001) {p_rounded <- sprintf("%.3f", round(x$p.value[[1]], 3))
                           p_no_leading_zero <- gsub("0\\.", ".", as.character(p_rounded))
                           p <- paste("p = ", p_no_leading_zero, sep = "")}
     if(x$p.value < .001) {p <- "p < .001"}
     r_apa_string <- paste("r(", df, ") = ", r, ", 95% CI [", r_lci, ", ", r_uci, "], ", p, sep = "")
     return(r_apa_string)
   }
   if(grepl("Two Sample t-test", x$method) &
     !grepl("Welch", x$method)) {
    require(MBESS)
    t <- sprintf("%.2f", round(x$statistic[[1]], 2))
    df <- x$parameter[[1]]
    if(x$p.value > .001) {p_rounded <- sprintf("%.3f", round(x$p.value[[1]], 3))
                          p_no_leading_zero <- gsub("0\\.", ".", as.character(p_rounded))
                          p <- paste("p = ", p_no_leading_zero, sep = "")}
    if(x$p.value < .001) {p <- "p < .001"}
    d_and_cis <- ci.smd(ncp = x$statistic[[1]],
                        n.1 = length(x$data$x),
                        n.2 = length(x$data$y),
                        conf.level = .95)
    d <- sprintf("%.2f", round(d_and_cis$smd, 2))
    d_lci <- sprintf("%.2f", round(d_and_cis$Lower.Conf.Limit.smd, 2))
    d_uci <- sprintf("%.2f", round(d_and_cis$Upper.Conf.Limit.smd, 2))
    t_apa_string <- paste("t(", df, ") = ", t, ", ", p, ", d = ", d, " 95% CI [", d_lci, ", ", d_uci, "]", sep = "")
    return(t_apa_string)
   }
   if(grepl(class(x)[1], "lm")) {
      require(broom)
      require(tidyverse)
      
      results <- 
        x %>%
        tidy()
      
      table <- 
        x %>%
        tidy() %>%
        mutate(result = NA) %>%
        dplyr::select(term, result)
      
      CIs <- x %>% confint()%>% as.data.frame %>% mutate(term = rownames(.))
      
      for (i in table$term) {
        estimate <- results %>% filter(term==i) %>% pull(estimate) %>% round (2) %>% formatC(digits=2, format='f')
        se <- results %>% filter(term==i) %>% pull(std.error) %>% round (2) %>% formatC(digits=2, format='f')
        CI_low <- CIs %>% filter(term==i) %>% pull(`2.5 %`) %>% round (2) %>% formatC(digits=2, format='f')
        CI_high <- CIs %>% filter(term==i) %>% pull(`97.5 %`) %>% round (2) %>% formatC(digits=2, format='f')
        statistic <- results %>% filter(term==i) %>% pull(statistic) %>% round (2) %>% formatC(digits=2, format='f')
        df <- summary(x)$df[2] 
        if(results$p.value[results$term==i] > .001)  {p_raw <- results$p.value[results$term==i]
                                                      p_rounded <- round(p_raw, 3)
                                                      p_no_leading_zero <- gsub("0\\.", ".", as.character(p_rounded))
                                                      p <- paste("p = ", p_no_leading_zero, sep = "")}
        if(results$p.value[results$term==i] < .001)  {p <- "p < .001"}
        string <- paste("beta = ", estimate, ", SE = ", se, ", 95% CI [", CI_low, ", ", CI_high, "], t(", df, ") = ", statistic, ", ", p, sep = "")
        table$result[table$term==i] <- string
      }
      return(table)
     }
   if(grepl(class(x)[1], "glm")) {
     require(broom)
     require(tidyverse)
     
     results <- 
       x %>%
       tidy()
     
     table <- 
       x %>%
       tidy() %>%
       mutate(result = NA) %>%
       dplyr::select(term, result)
     
     CIs <- x %>% confint()%>% as.data.frame %>% mutate(term = rownames(.))
     
     for (i in table$term) {
       estimate <- results %>% filter(term==i) %>% pull(estimate) %>% round (2) %>% formatC(digits=2, format='f')
       se <- results %>% filter(term==i) %>% pull(std.error) %>% round (2) %>% formatC(digits=2, format='f')
       CI_low <- CIs %>% filter(term==i) %>% pull(`2.5 %`) %>% round (2) %>% formatC(digits=2, format='f')
       CI_high <- CIs %>% filter(term==i) %>% pull(`97.5 %`) %>% round (2) %>% formatC(digits=2, format='f')
       statistic <- results %>% filter(term==i) %>% pull(statistic) %>% round (2) %>% formatC(digits=2, format='f')
       if(results$p.value[results$term==i] > .001)  {p_raw <- results$p.value[results$term==i]
                                                     p_rounded <- round(p_raw, 3)
                                                     p_no_leading_zero <- gsub("0\\.", ".", as.character(p_rounded))
                                                     p <- paste("p = ", p_no_leading_zero, sep = "")}
       if(results$p.value[results$term==i] < .001)  {p <- "p < .001"}
       string <- paste("b = ", estimate, ", SE = ", se, ", 95% CI [", CI_low, ", ", CI_high, "], Z = ", statistic, ", ", p, sep = "")
       table$result[table$term==i] <- string
      }
     return(table)
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
      lmer_summary <- summary(x)
      results_table <- as.data.frame(lmer_summary$coefficients)
      CIs_full <- as.data.frame(confint.merMod(x, method = "Wald"))
      CIs <- CIs_full[!is.na(CIs_full$`2.5 %`), ]
      results_table$CI_low <- CIs$`2.5 %`
      results_table$CI_high <- CIs$`97.5 %`
      lmer_apa_table <- data.frame(term = rownames(results_table),
                                   lmer_apa_string = NA)
      for (i in rownames(results_table)) {
        estimate_raw <- results_table$Estimate[rownames(results_table)==i]
        estimate <- sprintf("%.2f", round(estimate_raw, 2))
        se_raw <- results_table$`Std. Error`[rownames(results_table)==i]
        se <- sprintf("%.2f", round(se_raw, 2))
        CI_low_raw <- results_table$CI_low[rownames(results_table)==i]
        CI_low <- sprintf("%.2f", round(CI_low_raw, 2))
        CI_high_raw <- results_table$CI_high[rownames(results_table)==i]
        CI_high <- sprintf("%.2f", round(CI_high_raw, 2))
        if(results_table$`Pr(>|t|)`[rownames(results_table)==i] > .001) {p_raw <- results_table$`Pr(>|t|)`[rownames(results_table)==i]
                                                                         p_rounded <- round(p_raw, 3)
                                                                         p_no_leading_zero <- gsub("0\\.", ".", as.character(p_rounded))
                                                                         p <- paste("p = ", p_no_leading_zero, sep = "")}
        if(results_table$`Pr(>|t|)`[rownames(results_table)==i] < .001) {p <- "p < .001"}
        lmer_apa_string <- paste("coef = ", estimate, ", SE = ", se, ", 95% CI [", CI_low, ", ", CI_high, "], ", p, sep = "")
        lmer_apa_string_table$lmer_apa_string[lmer_apa_string_table$term==i] <- lmer_apa_string
      }
      return(lmer_apa_string_table)
    }
    if(grepl(class(x)[1], "metacor")) {
      require(meta)
      require(numform)
      require(tidyverse)
      table <- 
        data.frame(effect = c("fixed", "random"),
                   string = NA)
      
      fixed_r <- x$TE.fixed %>% z2cor() %>% round(2) %>% formatC(digits=2, format='f')
      fixed_lower <- x$lower.fixed %>% z2cor() %>% round(2) %>% formatC(digits=2, format='f')
      fixed_upper <- x$upper.fixed %>% z2cor() %>% round(2) %>% formatC(digits=2, format='f')
      fixed_z <- x$statistic.fixed %>% round(2) %>% formatC(digits=2, format='f')
      if(x$pval.fixed > .001) {fixed_p <- paste("p = ", x$pval.fixed %>% f_num(3), sep = "")}
      if(x$pval.fixed < .001) {fixed_p <- "p < .001"}
      
      random_r <- x$TE.random %>% z2cor() %>% round(2) %>% formatC(digits=2, format='f')
      random_lower <- x$lower.random %>% z2cor() %>% round(2) %>% formatC(digits=2, format='f')
      random_upper <- x$upper.random %>% z2cor() %>% round(2) %>% formatC(digits=2, format='f')
      random_z <- x$statistic.random %>% round(2) %>% formatC(digits=2, format='f')
      if(x$pval.random > .001) {random_p <- paste("p = ", x$pval.random %>% f_num(3), sep = "")}
      if(x$pval.random < .001) {random_p <- "p < .001"}
      
      table$string[1] <- paste("r = ", fixed_r, ", 95% CI [", fixed_lower, ", ", fixed_upper, "], Z = ", fixed_z, ", ", fixed_p, sep = "")
      table$string[2] <- paste("r = ", random_r, ", 95% CI [", random_lower, ", ", random_upper, "], Z = ", random_z, ", ", random_p, sep = "")
      
      return(table)
     }
    if(grepl(class(x)[1], "metacont") | 
       grepl(class(x)[1], "metamean")) {
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
