

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

