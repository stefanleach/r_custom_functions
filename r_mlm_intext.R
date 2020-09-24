#MLM in-text print function
printlmer <- function(x) {
  require(numform)
  require(tidyverse)
  tidy_tibble <- 
    x %>%
    tidy() %>%
    mutate(result = NA) %>%
    filter(effect=="fixed") %>%
    dplyr::select(term, result)
  
  x_summary <- summary(x)
  
  for (i in tidy_tibble$term) {
    estimate <- formatC(round(x_summary$coefficients[i, ][[1]], 2), digits=2, format='f')
    se <- formatC(round(x_summary$coefficients[i, ][[2]], 2), digits=2, format='f')
    CIs <- confint.merMod(x, method = "Wald")
    CI_low <- formatC(round(CIs[i, ][1], 2), digits=2, format='f')
    CI_high <- formatC(round(CIs[i, ][2], 2), digits=2, format='f')
    p <- x_summary$coefficients[i, ][[5]]
    estimate_print <- paste(estimate,", ", sep = "")
    se_print <- paste("SE = ", se,", ", sep = "")
    CI_low_print <- paste("95% CI [", CI_low, ", ", sep = "")
    CI_high_print <- paste(CI_high, "], ", sep = "")
    if(p > .001) {p_print <- paste("p = ", f_num(p, 3), sep = "")}
    if(p < .001) {p_print <- paste("p < .001", sep = "")}
    result_string <- paste(estimate_print, se_print, CI_low_print, CI_high_print, p_print, sep = "")
    tidy_tibble$result[tidy_tibble$term==i] <- result_string
  }
  return(tidy_tibble)
}
