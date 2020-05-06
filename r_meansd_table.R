table_descriptives <- function(data, caption){
  require(Hmisc)
  require(papaja)
  require(stringr)
  # capitalize first letter in all strings
  for (i in 1:ncol(data)) {
    colnames(data)[i] <- capitalize(colnames(data)[i])
    for (y in 1:nrow(data)) {
      data[[i]][y] <- capitalize(data[[i]][y])
    }
  }
  # replace "_" with " "
  for (i in 1:ncol(data)) {
    colnames(data)[i] <- str_replace(colnames(data)[i], "_", " ")
    for (y in 1:nrow(data)) {
      data[[i]][y] <- str_replace(data[[i]][y], "_", " ")
    }
  }
  # apa style
  table <- apa_table(data, 
                     caption = caption,
                     format = "markdown",
                     escape = FALSE,
                     note = "Values reflect means (and standard deviations)")
  ## return table
  return(table)
}