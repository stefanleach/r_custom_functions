#MLM coding scheme table print function
codinglmer <- function(x) { # x = data"
  require(papaja)
  #requires terms be ascribed attribute named "coding"
  data <- x
  #get attributes
  attributes <- data.frame()
  for (i in 1:length(colnames(data))) {
    if(is.null(attr(data[, i], "coding"))==FALSE){
      name <- attr(data[, i], "name")
      coding <- attr(data[, i], "coding")
      tempdataframe <- data.frame("Term" = name, "Coding" = coding, stringsAsFactors = FALSE)
      attributes <- rbind(attributes, tempdataframe)}}
  #put attributes in table
  table <- apa_table(attributes, 
                     caption = "Coding scheme adopted for multilevel models",
                     align = c('l', 'l'),
                     format = "markdown",
                     escape = FALSE)
  return(table) }