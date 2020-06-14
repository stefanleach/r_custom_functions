wordcount <- function(x, y) { # x = vector with words, y = vector with text
  require(stringr)
  words <- tolower(x)
  texts <- tolower(y)
  temp <- data.frame(v1 = 1:length(texts))
  for (i in 1:length(texts)) {
    for (y in 1:length(words)) {
      target_word <- paste("\\b", words[y], "\\b", sep = "")
      temp[i, y] <-  str_count(texts[i], target_word)
    }
  }
  colnames(temp) <- words
  return(temp)
}
