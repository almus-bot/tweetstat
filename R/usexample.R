# Paquetes para extraer info de twitter y textos
library(rtweet)
library(RCurl)
library(RJSONIO)
library(stringr)

# Funciones para procesar texto/nube de palabras
library(tm)
library(wordcloud)
#library(SnowballC)
library(ggplot2)

# El usuario debe tener una app en twitter para el uso de rtweet
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

# Create token to use rtweet
twitter_token <- create_token(app = appname, consumer_key = key, consumer_secret = secret )
## NOTA: Se abrirá la pagina de twitter para confirmar la autorización ##

user <- "genbeta"  # write here a twitter username to analyse.

source("R/resume.R")   # summary function
source("R/cleantext.R") # function to get tidy words or tweets
source("R/makecorpus.R") # function to process corpus from text (words and frequencies)
source("R/view_words.R") # plot word cloud or histogram from text 
source("R/sentiment.R") # performs sentiment analysis by indicoio, plot sentiment evolution in time.

datos <- resume(user)
cleaned_text <- cleantext(tw$text)
cleaned_sentences <- cleantext(tw$text, only_words = FALSE)
corpus_text <- makecorpus(cleaned_text)
view_words(corpus_text, type="cloud")
view_words(corpus_text, type="hist")

api_key = '7d9bb1d258a407b35a7a40787b625912'
sentimentplot(tw$text, tw$created_at, api_key, "es")


