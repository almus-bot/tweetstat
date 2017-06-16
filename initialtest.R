# Paquetes para extraer info de twitter y textos
library(rtweet)
library(RCurl)
library(RJSONIO)
library(stringr)

# Funciones para procesar texto/nube de palabras
library(tm)
library(wordcloud)
library(SnowballC)
library(ggplot2)

# Datos de la App de prueba de twitter
# Coloque aquí el nombre de su app de twitter
appname <- ""   
# Coloque aquí el "Consumer Key (API Key)" de la app
key <- ""   
# Coloque aquí el "Consumer Secret (API Secret)" de la app
secret <- ""  
# Coloque aquí el "Access Token" personal de la app 
token <- ""
# Coloque aquí el "Access Token Secret" personal de la app 
token_secret <- ""

# Creando token para usar con rtweet
twitter_token <- create_token(app = appname, consumer_key = key, consumer_secret = secret )
## NOTA: Se abrirá la pagina de twitter para confirmar la autorización ##

# Prueba: buscando tweets (con retweets/sin retweets) con paquete rtweet
tw1 <- search_tweets("ciencia de datos", n=10, token = twitter_token)
tw2 <- search_tweets("ciencia de datos -filter:retweets", n=10, token = twitter_token)

user <- "genbeta"  # coloque aquí el usuario que quiera analizar
# Obteniendo tweets de un usuario particular (timeline)

resume <- function(user, rt=FALSE){
  
  if (rt == TRUE){
    tw <- get_timeline(user, n=10)
    ## Cuantos son retweets?
    num_rtw <- sum(tw$is_retweet)
    
  }else{
    tw <- get_timeline(user, n=10, include_rts = FALSE) # sin retweets
    num_rtw <- 'No aplica'
  }
  
  # Mentions (a quién y cuantos)
  mentions <- na.omit(tw$mentions_screen_name)
  corp_ment <- processcorpus(mentions)
  num_mention <- dim(corp_ment)[1]
  
  # Last tweet 
  last_tweet <- tw$text[1]
  is_rtweet <- tw$is_retweet[1]
  
  user_info <- users_data(tw[1,])[1,]# Extrayendo datos del usuario
  
  # Reduciendo el data frame con datos considerados para twitterstat
  user_info <- user_info[,c("name","screen_name", "location","description","protected",
                                    "followers_count","friends_count", "created_at","favourites_count",
                                    "time_zone","verified","statuses_count", "lang", "profile_image_url")]

  user_info <- data.frame(user_info,num_rtw, num_mention,last_tweet, is_rtweet)  
  
  }

# Ejemplo de uso
datos <- resume("mariangely_s", TRUE)

# Esta función limpia el texto de los tweets para obtener palabras frecuentes.
cleantext <- function(s, only_words = TRUE){
  
    chat_text <- gsub('\\p{So}|\\p{Cn}', '', s, perl = TRUE)
    chat_text <- gsub("http[^[:space:]]*", " ", chat_text)   # Elimina URL's
    chat_text = gsub('\\b+RT', '', chat_text) ## Elimina RT
    chat_text = gsub('@\\S+', '', chat_text) ## Elimina Mentions
    chat_text = gsub('#\\S+', '', chat_text) ## Elimina Hashtags
    chat_text <- gsub("[[:cntrl:]]", " ", chat_text)  # ELimina caracteres de control
  
  if (only_words == TRUE) {
    chat_text <- gsub("[[:digit:]]", " ", chat_text)  # Elimina números/dígitos
    chat_text <- gsub("[[:punct:]]", " ", chat_text)  # Elimina caracteres de puntuación ortográfica
    chat_text <- tolower(chat_text)  # Convierte todo el texto a minúsculas
    chat_text <- removeWords(chat_text, words = stopwords("spanish"))  # Elimina conectores y demás palabras sin relevancia 
    chat_text <- removeWords(chat_text, words = stopwords("english"))  # Elimina conectores y demás palabras sin relevancia (en inglés)
    # Elimina palabras adicionales (Éstas palabras hay que cambiarlas a conveniencia)
    chat_text <- removeWords(chat_text, words = c("usted", "pues", "tal", "tan", "asi", "mas","dijo", "como", "dije" , "digo",
                                                  "entonces", "aunque", "ahi", "aqui"))
    }
    chat_text <- stripWhitespace(chat_text)  # Elimina espacios en blanco sobrantes
}  


# Funcion para generar el Corpus necesario para obtener los términos y las frecuencias.
processcorpus <- function(text){
    Encoding(text) <- "UTF-8"   # Asegura la codificación utf8
    corp <- Corpus(DataframeSource(data.frame(text))) # Genera el corpus desde el texto plano
    tdm <- TermDocumentMatrix(corp)  # Genera la lista de palabras con frecuencias
    text.m <- as.matrix(tdm)
    text.v <- sort(rowSums(text.m),decreasing=TRUE)  # Ordena la lista según la frecuencia
    text.d <- data.frame(word = names(text.v),freq=text.v) # Converte en data frame para mejor manejo
} 

# Procesandos los tweets con retweets incluidos
text <- cleantext(tweets$text)
corp <- processcorpus(text)

# Procesandos los tweets sin retweets incluidos
text1<- cleantext(tweets1$text)
corp1 <- processcorpus(text1)

# Preparando el área para graficar
layout(matrix(c(1,2,3,4), nrow=2), heights=c(1, 4, 1, 4))
par(mar=rep(0,4))

plot.new()
text(x=0.5, y=0.3, "Con retweets")
# Generando nube de palabras para las palabras frecuentes (tweets con retweets)
wordcloud(words = corp$word, freq= corp$freq, min.freq = 2, scale=c(2,.4),
          max.words = 50, random.order = FALSE, rot.per = 0.4,
          colors = brewer.pal(8, "Dark2")) 

plot.new()
text(x=0.5, y=0.3, "Sin retweets")
# Generando nube de palabras para las palabras frecuentes (tweets sin retweets)
wordcloud(words = corp1$word, freq= corp1$freq, min.freq = 2, scale=c(2,.4),
          max.words = 50, random.order = FALSE, rot.per = 0.4,
          colors = brewer.pal(8, "Dark2"))

# HISTOGRAMA DE LAS 10 PALABRAS MÁS FRECUENTES
g1 <- ggplot(corp[1:10,],aes(word, freq))+
    geom_col()

g2 <- ggplot(corp1[1:10,],aes(word, freq))+
    geom_col()

require(gridExtra)
grid.arrange(g1,g2, nrow=2)

# prueba de wordcloud
cloud <- function(corpus){

  wordcloud(words = corpus[,1], freq= corpus[,2], min.freq = 2, scale=c(2,.4),
          max.words = 100, random.order = FALSE, rot.per = 0.4,
          colors = brewer.pal(8, "Dark2")) 

}

## SENTIMENT ANALYSIS TEST
# USando la api de indicoio

source("https://bioconductor.org/biocLite.R")
biocLite("EBImage")

library(devtools)
install_github("IndicoDataSolutions/IndicoIo-R")

library(indicoio)

# single example
sentiment(
  "I love writing code!",
  api_key = '7d9bb1d258a407b35a7a40787b625912'
)

# batch example
sentiment(
  c(
    "I love writing code!",
    "Alexander and the Terrible, Horrible, No Good, Very Bad Day"
  ),
  api_key = '7d9bb1d258a407b35a7a40787b625912'
)



sentiment(
  "Amo escribir código!",
  api_key = '7d9bb1d258a407b35a7a40787b625912', language = "es"
)


