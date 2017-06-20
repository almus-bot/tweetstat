#' A function for perform sentiment analysis
#'
#' This function takes text from tweets to plot the sentiment's evolution across time.
#' Uses indicoio API.
#' @param text Vector of character with cleaned tweets (plain sentences) to analyse.
#' @param dates Vector of corresponding dates when tweets were created at.
#' @param key Character string key necessary to use indicoio API.
#' @param lang Specified text's language to use indicoio API.
#' @export
#' @examples
#' sentimentplot(text, created_at, api_key, "en")

sentimentplot <- function(text, dates, key, lang){
  
  sent_analysis <- indicoio::sentiment(text,  api_key = key, language = "es")
  
  # Plotting sentiment evolution 
  plot(dates, unlist(sent_analysis), type = "l")
  
}
  
keywords <- function(text, key){
  
  key_w <- keywords(
    text,
    api_key = '7d9bb1d258a407b35a7a40787b625912',
    version = 2
  )
  
  # Not for spanish
  topics <- text_tags(
    text,
    api_key = '7d9bb1d258a407b35a7a40787b625912'
  )

}


