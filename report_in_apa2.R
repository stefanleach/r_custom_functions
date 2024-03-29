report_estimate_in_apa <- 
  function(x) {
    estimate_apa <- sprintf("%.2f", round(x, 2))
    estimate_apa
  }

report_r_in_apa <- 
  function(x) {
    r_apa <- sprintf("%.2f", round(x, 2))
    r_apa <- gsub("0\\.", ".", as.character(r_apa))
    r_apa
  }

report_p_in_apa <- 
  function(x) {
    if(x > .001) {
      p_rounded <- sprintf("%.3f", round(x, 3))
      p_no_leading_zero <- gsub("0\\.", ".", as.character(p_rounded))
      p_apa <- paste("p = ", p_no_leading_zero, sep = "")
    }
    if(x < .001) {
      p_apa <- "p < .001"
    }
    p_apa
  }

report_cor_in_apa <- 
  function(x) {
    r <- report_r_in_apa(x$estimate[[1]])
    r_lci <- report_r_in_apa(x$conf.int[1])
    r_uci <- report_r_in_apa(x$conf.int[2])
    t <- report_estimate_in_apa(x$statistic[[1]])
    df <- x$parameter[[1]]
    p <- report_p_in_apa(x$p.value[[1]])
    r_apa_string <- paste("r(", df, ") = ", r, ", 95% CI [", r_lci, ", ", r_uci, "], ", p, sep = "")
    r_apa_string
  }

report_t_one_paired_in_apa <- 
  function(x) {
    require(MBESS)
    t <- report_estimate_in_apa(x$statistic[[1]])
    df <- x$parameter[[1]]
    p <- report_p_in_apa(x$p.value[[1]])
    d <- report_estimate_in_apa(x$statistic[[1]]/sqrt(x$parameter[[1]]+1))
    t_and_cis <- conf.limits.nct(ncp = x$statistic[[1]],
                                 df = x$parameter[[1]],
                                 conf.level = .95)
    t_lci <- t_and_cis$Lower.Limit[1]
    t_uci <- t_and_cis$Upper.Limit[1]
    d_lci <- report_estimate_in_apa(t_lci/sqrt(x$parameter[[1]]+1))
    d_uci <- report_estimate_in_apa(t_uci/sqrt(x$parameter[[1]]+1))
    t_apa_string <- paste("t(", df, ") = ", t, ", ", p, ", dz = ", d, ", 95% CI [", d_lci, ", ", d_uci, "]", sep = "")
    t_apa_string
  }

report_t_inde_in_apa <- 
  function(x) {
    require(MBESS)
    t <- report_estimate_in_apa(x$statistic[[1]])
    df <- x$parameter[[1]]
    p <- report_p_in_apa(x$p.value[[1]])
    d_and_cis <- ci.smd(ncp = x$statistic[[1]],
                        n.1 = length(x$data$x),
                        n.2 = length(x$data$y),
                        conf.level = .95)
    d <- report_estimate_in_apa(d_and_cis$smd)
    d_lci <- report_estimate_in_apa(d_and_cis$Lower.Conf.Limit.smd)
    d_uci <- report_estimate_in_apa(d_and_cis$Upper.Conf.Limit.smd)
    t_apa_string <- paste("t(", df, ") = ", t, ", ", p, ", ds = ", d, ", 95% CI [", d_lci, ", ", d_uci, "]", sep = "")
    t_apa_string
  }

report_lm_in_apa <- 
  function(x) {
    require(stats)
    lm_summary <- summary(x)
    results_table <- as.data.frame(lm_summary$coefficients)
    CIs_full <- as.data.frame(confint(x))
    CIs <- CIs_full[!is.na(CIs_full$`2.5 %`), ]
    results_table$CI_low <- CIs$`2.5 %`
    results_table$CI_high <- CIs$`97.5 %`
    lm_apa_string_table <- data.frame(term = rownames(results_table),
                                      apa_string = NA)
    for (i in rownames(results_table)) {
      estimate <- report_estimate_in_apa(results_table$Estimate[rownames(results_table)==i])
      se <- report_estimate_in_apa(results_table$`Std. Error`[rownames(results_table)==i])
      CI_low <- report_estimate_in_apa(results_table$CI_low[rownames(results_table)==i])
      CI_high <- report_estimate_in_apa(results_table$CI_high[rownames(results_table)==i])
      p <- report_p_in_apa(results_table$`Pr(>|t|)`[rownames(results_table)==i])
      apa_string <- paste("coef = ", estimate, ", SE = ", se, ", 95% CI [", CI_low, ", ", CI_high, "], ", p, sep = "")
      lm_apa_string_table$apa_string[lm_apa_string_table$term==i] <- apa_string
    }
    lm_apa_string_table
  }

report_anova_in_apa <- 
  function(x) {
    table <- apa(x)
    for (i in 1:nrow(table)) {
      table$text[i] <- str_replace_all(table$text[i], "\\*", "")
      table$text[i] <- str_replace_all(table$text[i], "\\$", "")
      table$text[i] <- str_replace_all(table$text[i], "\\\\", "")
    } 
    return(table)
  }

report_lmer_in_apa <- 
  function(x) {
    require(lmerTest)
    lmer_summary <- summary(x)
    results_table <- as.data.frame(lmer_summary$coefficients)
    CIs_full <- as.data.frame(confint.merMod(x, method = "Wald"))
    CIs <- CIs_full[!is.na(CIs_full$`2.5 %`), ]
    results_table$CI_low <- CIs$`2.5 %`
    results_table$CI_high <- CIs$`97.5 %`
    lmer_apa_string_table <- data.frame(term = rownames(results_table),
                                        apa_string = NA)
    for (i in rownames(results_table)) {
      estimate <- report_estimate_in_apa(results_table$Estimate[rownames(results_table)==i])
      se <- report_estimate_in_apa(results_table$`Std. Error`[rownames(results_table)==i])
      CI_low <- report_estimate_in_apa(results_table$CI_low[rownames(results_table)==i])
      CI_high <- report_estimate_in_apa(results_table$CI_high[rownames(results_table)==i])
      p <- report_p_in_apa(results_table$`Pr(>|t|)`[rownames(results_table)==i])
      apa_string <- paste("coef = ", estimate, ", SE = ", se, ", 95% CI [", CI_low, ", ", CI_high, "], ", p, sep = "")
      lmer_apa_string_table$apa_string[lmer_apa_string_table$term==i] <- apa_string
    }
    lmer_apa_string_table
  }

z2cor <- 
  function(x) {
    res <- (exp(2 * x) - 1) / (exp(2 * x) + 1)
    res
  }

report_metacor_in_apa <- 
  function(x) {
    metacor_apa_string_table <- data.frame(effect = c("fixed", "random"),
                                           apa_string = NA)
    fixed_r <- report_r_in_apa(z2cor(x$TE.fixed))
    fixed_lower <- report_r_in_apa(z2cor(x$lower.fixed))
    fixed_upper <- report_r_in_apa(z2cor(x$upper.fixed))
    fixed_z <- report_estimate_in_apa(x$statistic.fixed)
    fixed_p <- report_p_in_apa(x$pval.fixed)
    random_r <- report_r_in_apa(z2cor(x$TE.random))
    random_lower <- report_r_in_apa(z2cor(x$lower.random))
    random_upper <- report_r_in_apa(z2cor(x$upper.random))
    random_z <- report_estimate_in_apa(x$statistic.random)
    random_p <- report_p_in_apa(x$pval.random)
    metacor_apa_string_table$apa_string[1] <- paste("r = ", fixed_r, ", 95% CI [", fixed_lower, ", ", fixed_upper, "], Z = ", fixed_z, ", ", fixed_p, sep = "")
    metacor_apa_string_table$apa_string[2] <- paste("r = ", random_r, ", 95% CI [", random_lower, ", ", random_upper, "], Z = ", random_z, ", ", random_p, sep = "")
    metacor_apa_string_table
  }

report_metamean_in_apa <- 
  function(x) {
    metamean_apa_string_table <- data.frame(effect = c("fixed", "random"),
                                            apa_string = NA)
    fixed_d <- report_estimate_in_apa(x$TE.fixed)
    fixed_lower <- report_estimate_in_apa(x$lower.fixed)
    fixed_upper <- report_estimate_in_apa(x$upper.fixed)
    fixed_z <- report_estimate_in_apa(x$statistic.fixed)
    fixed_p <- report_p_in_apa(x$pval.fixed)
    random_d <- report_estimate_in_apa(x$TE.random)
    random_lower <- report_estimate_in_apa(x$lower.random)
    random_upper <- report_estimate_in_apa(x$upper.random)
    random_z <- report_estimate_in_apa(x$statistic.random)
    random_p <- report_p_in_apa(x$pval.random)
    metamean_apa_string_table$apa_string[1] <- paste("d = ", fixed_d, ", 95% CI [", fixed_lower, ", ", fixed_upper, "], Z = ", fixed_z, ", ", fixed_p, sep = "")
    metamean_apa_string_table$apa_string[2] <- paste("d = ", random_d, ", 95% CI [", random_lower, ", ", random_upper, "], Z = ", random_z, ", ", random_p, sep = "")
    metamean_apa_string_table
  }

report_in_apa <- 
  function(x) {
    if(inherits(x, "htest")) {
      if(grepl(x$method, "Pearson's product-moment correlation")) {
        report_cor_in_apa(x)
      } 
      else if(grepl("One Sample t-test", x$method) | grepl("Paired t-test", x$method)) {
        report_t_one_paired_in_apa(x)
      }
      else if(grepl("Two Sample t-test", x$method) & !grepl("Welch", x$method) & class(x$data)=="list") {
        report_t_inde_in_apa(x)
      }
      else {
        stop("I do not support this method")
      }
    }
    else if(grepl(class(x)[1], "lm")) {
      report_lm_in_apa(x)
    }
    else if(grepl(class(x)[1], "lmerModLmerTest")) {
      report_lmer_in_apa(x)
    }
    else if(grepl(class(x)[1], "afex_aov")) {
      report_anova_in_apa(x)
    }
    else if(grepl(class(x)[1], "metacor")) {
      report_metacor_in_apa(x)
    }
    else if(grepl(class(x)[1], "metacont") | grepl(class(x)[1], "metamean")) {
      report_metamean_in_apa(x)
    }
    else {
      stop("I do not support this method")
    }
  } 