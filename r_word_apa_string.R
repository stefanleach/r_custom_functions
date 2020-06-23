word_apa_string <- function (string) {
  require(pander)
  require(rmarkdown)
  tmp <- tempfile("print_word_apa", fileext = ".md")
  sink(tmp)
  print(string)
  sink()
  outfile <- render(tmp, output_format = "word_document", quiet = TRUE)
  openFileInOS(outfile)
}