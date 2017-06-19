#' A Function for extract words and its frequencies
#'
#' This function generate the necessary corpus from text, to get words and its frequencies
#' from text.
#' @param text text Vector with strings of singles words to count.
#' @export
#' @examples
#' makecorpus(vector)

makecorpus <- function(text){
  Encoding(text) <- "UTF-8"   # Ensure utf8 encoding
  corp <- Corpus(DataframeSource(data.frame(text))) # create corpus from plain text
  tdm <- TermDocumentMatrix(corp)  # Generate words list with its frequencies.
  text.m <- as.matrix(tdm)
  text.v <- sort(rowSums(text.m),decreasing=TRUE)  # sort list of words by frequencies.
  text.d <- data.frame(word = names(text.v),freq=text.v) # convert to dataframe 
} 