#MLM in-text print function
printlmer <- function(x, y) { # x = test results, y = "term"
  require(numform)
  x_summary <- summary(x)
  estimate <- formatC(round(x_summary$coefficients[y, ][[1]], 2), digits=2, format='f')
  se <- formatC(round(x_summary$coefficients[y, ][[2]], 2), digits=2, format='f')
  CIs <- confint.merMod(x, method = "Wald")
  CI_low <- formatC(round(CIs[y, ][1], 2), digits=2, format='f')
  CI_high <- formatC(round(CIs[y, ][2], 2), digits=2, format='f')
  p <- x_summary$coefficients[y, ][[5]]
  estimate_print <- paste(estimate,", ", sep = "")
  se_print <- paste("SE = ", se,", ", sep = "")
  CI_low_print <- paste("95% CI [", CI_low, ", ", sep = "")
  CI_high_print <- paste(CI_high, "], ", sep = "")
  if(p > .001) {p_print <- paste("p = ", f_num(p, 3), sep = "")}
  if(p < .001) {p_print <- paste("p < .001", sep = "")}
  results_print <- paste(estimate_print, se_print, CI_low_print, CI_high_print, p_print, sep = "")
  return(results_print)
}
