#MLM table print function
tablelmer <- function(x, y) { # x = test results, y = "caption"
  require(papaja)
  require(broom.mixed)
  require(numform)
  require(stringr)
  summaryresults <- summary(x)
  tidyresults <- tidy(x)
  figurecaption <- y
  #Compute confidence intervals
  confidence_intervals <- confint.merMod(x, method = "Wald")
  confidence_intervals <- tidy(confidence_intervals)
  confidence_intervals <- confidence_intervals[is.na(confidence_intervals$X2.5..)==FALSE, ]
  confidence_intervals <- data.frame("term" = confidence_intervals[[1]],
                                     "CILower" = confidence_intervals[[2]],
                                     "CIUpper" = confidence_intervals[[3]])
  #Merge confidence intervals
  tidyresults$CILower <- c(NA)
  tidyresults$CIUpper <- c(NA)
  for (i in 1:length(confidence_intervals$term)){
    tidyresults$CILower[i] <- confidence_intervals$CILower[i]
    tidyresults$CIUpper[i] <- confidence_intervals$CIUpper[i]
  }
  #Sort Columns
  tidyresults <- tidyresults[,c(1,2,3,4,5,9,10,6,7,8)]
  #Tidy intercept terms
  for (i in 1:length(tidyresults$term)) {
    tidyresults$term[i] <- str_replace(tidyresults$term[i], "\\(", "")
    tidyresults$term[i] <- str_replace(tidyresults$term[i], "\\)", "")
    tidyresults$term[i] <- str_replace(tidyresults$term[i], "\\.", ",")
  }
  #Tidy random effects terms
  for (i in 1:length(tidyresults$term[tidyresults$effect=="ran_pars"])) {
    tidyresults$term[tidyresults$effect=="ran_pars"][i] <- str_replace_all(tidyresults$term[tidyresults$effect=="ran_pars"][i], "sd__", "SD~")
    tidyresults$term[tidyresults$effect=="ran_pars"][i] <- str_replace_all(tidyresults$term[tidyresults$effect=="ran_pars"][i], "cor__", "COR~")
    tidyresults$term[tidyresults$effect=="ran_pars"][i] <- paste(tidyresults$term[tidyresults$effect=="ran_pars"][i], "~", sep = "")
  }
  #Tidy interaction terms
  for (i in 1:length(tidyresults$term)) {
    tidyresults$term[i] <- str_replace_all(tidyresults$term[i], ":", " X ")
  }
  #Get attributes
  attributes <- data.frame()
  for (i in 1:length(colnames(data))) {
    if(is.null(attr(data[, i], "coding"))==FALSE){
      variable <- colnames(data)[i]
      name <- attr(data[, i], "name")
      coding <- attr(data[, i], "coding")
      tempdataframe <- data.frame("variable" = variable, "name" = name, "coding" = coding, stringsAsFactors = FALSE)
      attributes <- rbind(attributes, tempdataframe)}} 
  
  #Tidy column names
  for (i in 1:length(attributes$variable)) {
    tidyresults$term <- str_replace_all(tidyresults$term, attributes$variable[i], attributes$name[i])
    tidyresults$group <- str_replace_all(tidyresults$group, attributes$variable[i], attributes$name[i])
  }
  #Tidy p-values
  for (i in 1:length(tidyresults$term[tidyresults$effect=="fixed"])) {
    p <- tidyresults$p.value[i]
    if(p > .001) {tidyresults$p.value[i] <- f_num(p, 3)}
    if(p < .001) {tidyresults$p.value[i] <- "< .001"}
  }
  #Tidy fixed and random effects names
  tidyresults$effect[tidyresults$effect=="fixed"] <- "Fixed"
  tidyresults$effect[tidyresults$effect=="ran_pars"] <- "Random"
  #Tidy column names for rMarkdown
  colnames(tidyresults) <- c("Effect", "Group", "Term", "Estimate", "SE", "CI~Lower~", "CI~Upper~", "*t*", "*df*","*p*")
  
  #Set table parameters
  table <- apa_table(tidyresults, 
                     caption = figurecaption,
                     align = c('l', 'l', 'l', 'r', 'r', 'r', 'r', 'r', 'r', 'r'),
                     format = "markdown",
                     escape = FALSE,
                     note = "Model fit using the *lme4* R-package (Bates et al., 2015). 
                     Regression coefficients are based on restricted estimates (REML). 
                     *P*-values and confidence intervals are obtained via Satterthwaite 
                     and Wald approximation, respectively")
  return(table) }
