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


