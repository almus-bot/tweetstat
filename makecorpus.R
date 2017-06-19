# Funcion para generar el Corpus necesario para obtener los términos y las frecuencias.
makecorpus <- function(text){
  Encoding(text) <- "UTF-8"   # Asegura la codificación utf8
  corp <- Corpus(DataframeSource(data.frame(text))) # Genera el corpus desde el texto plano
  tdm <- TermDocumentMatrix(corp)  # Genera la lista de palabras con frecuencias
  text.m <- as.matrix(tdm)
  text.v <- sort(rowSums(text.m),decreasing=TRUE)  # Ordena la lista según la frecuencia
  text.d <- data.frame(word = names(text.v),freq=text.v) # Converte en data frame para mejor manejo
} 