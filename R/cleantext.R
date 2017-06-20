#' A Function for clean texts
#'
#' This function allows you to get single words from texts, or get 
#' complete sentences from tweets, without urls and non necessary symbols
#' @param s Vector containing strings to clean.
#' @param only_words If TRUE, return only single words from cleaned text (default). If FALSE, return complete
#' sentences.
#' @export
#' @examples
#' cleantext(string)

cleantext <- function(s, only_words = TRUE){
  
  chat_text <- gsub('\\p{So}|\\p{Cn}', '', s, perl = TRUE)
  chat_text <- gsub("http[^[:space:]]*", " ", chat_text)   # Elimina URL's
  chat_text <- gsub('\\b+RT', '', chat_text) ## Elimina RT
  chat_text <- gsub('@\\S+', '', chat_text) ## Elimina Mentions
  chat_text <- gsub('#\\S+', '', chat_text) ## Elimina Hashtags
  chat_text <- gsub("[[:cntrl:]]", " ", chat_text)  # ELimina caracteres de control
  
  if (only_words == TRUE) {
    chat_text <- gsub("[[:digit:]]", " ", chat_text)  # Elimina números/dígitos
    chat_text <- gsub("[[:punct:]]", " ", chat_text)  # Elimina caracteres de puntuación ortográfica
    chat_text <- tolower(chat_text)  # Convierte todo el texto a minúsculas
    chat_text <- tm::removeWords(chat_text, words = stopwords("spanish"))  # Elimina conectores y demás palabras sin relevancia 
    chat_text <- tm::removeWords(chat_text, words = stopwords("english"))  # Elimina conectores y demás palabras sin relevancia (en inglés)
    # Elimina palabras adicionales (Éstas palabras hay que cambiarlas a conveniencia)
    chat_text <- tm::removeWords(chat_text, words = c("usted", "pues", "tal", "tan", "asi", "mas","dijo", "como", "dije" , "digo",
                                                  "entonces", "aunque", "ahi", "aqui"))
  }
  chat_text <- tm::stripWhitespace(chat_text)  # Elimina espacios en blanco sobrantes
}  