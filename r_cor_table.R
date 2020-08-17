#Correlation table print function
cortable <-function(x, method=c("pearson", "spearman"), caption, landscape,
                    removeTriangle=c("upper", "lower"), result=c("none", "html", "latex")){
  #Compute correlation matrix
  require(Hmisc)
  require(numform)
  require(tidyverse)
  dataframe <- x
  x <- as.matrix(x)
  correlation_matrix<-rcorr(x, type=method[1])
  R <- correlation_matrix$r # Matrix of correlation coeficients
  p <- correlation_matrix$P # Matrix of p-value 
  
  ## Define notions for significance levels; spacing is important.
  mystars <- ifelse(p < .001, "*** ", ifelse(p < .01, "**  ", ifelse(p < .05, "*   ", "    ")))
  
  ## trunctuate the correlation matrix to two decimal
  R <- format(round(cbind(rep(-1.11, ncol(x)), R), 2))[,-1]
  for (i in 1:length(colnames(R))) {
    R[, i] <- f_num(R[, i], 2)
  }
  
  ## build a new matrix that includes the correlations with their apropriate stars
  Rnew <- matrix(paste(R, mystars, sep=""), ncol=ncol(x))
  diag(Rnew) <- paste(diag(R), " ", sep="")
  rownames(Rnew) <- colnames(x)
  colnames(Rnew) <- paste(colnames(x), "", sep="")
  
  ## remove upper triangle of correlation matrix
  if(removeTriangle[1]=="upper"){
    Rnew <- as.matrix(Rnew)
    Rnew[upper.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove lower triangle of correlation matrix
  else if(removeTriangle[1]=="lower"){
    Rnew <- as.matrix(Rnew)
    Rnew[lower.tri(Rnew, diag = TRUE)] <- ""
    Rnew <- as.data.frame(Rnew)
  }
  
  ## remove last column and return the correlation matrix
  Rnew <- cbind(Rnew[1:length(Rnew)-1])
  
  ## add means and sds
  for (i in 1:length(colnames(x))) {
    Rnew$mean[i] <- mean(x[, i], na.rm = TRUE)
    Rnew$sds[i] <- sd(x[, i], na.rm = TRUE)
  }
  ## reorder columns
  column_length <- length(colnames(Rnew))
  column_length_meansd <- length(colnames(Rnew)) - 1
  column_length_corr <- column_length - 2
  column_meansd <- c(column_length_meansd:column_length)
  column_corr <- c(1:column_length_corr)
  order <- c(column_meansd, column_corr)
  Rnew <- Rnew[,c(order)]
  
  ## tidy column names
  column_names <- c("m", "sd", column_corr)
  colnames(Rnew) <- column_names
  
  ## round means and sds
  Rnew[, 1:2] <- Rnew[, 1:2] %>% round(digits=2)
  
  ## add number to row variables
  for (i in 1:length(row.names(Rnew))) {
    row.names(Rnew)[i] <- paste(i, row.names(Rnew)[i])
  }
  
  ## return table
  return(Rnew)
} 
