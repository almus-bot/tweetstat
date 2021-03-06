#' Plot generator for tweetstat
#'
#' This function takes words and its frequencies to generate wordclouds or histograms.
#' Comparing two sets of words it's available.
#' @param words1 dataframe with set of words in its first column, set of frequencies in its second column.
#' @param words2 Optional: dataframe with same structure of 'words1', used when 'compare"==TRUE. Default = NULL.
#' @param compare TRUE when you want to compare two sets of words. FALSE will generate only one plot. Default = FALSE.
#' @param type Use "cloud" to generate wordcloud, or "hist" for histograms. Default = "cloud".
#' @export
#' @examples
#' view_words(words, other_set, compare = TRUE, type = "cloud")


view_words <- function(words1, words2=NULL, compare=FALSE, type="cloud"){
  
  if (type=="cloud") {
  
    if (compare == FALSE){
      wordcloud::wordcloud(words = words1$word, freq= words1$freq, min.freq = 2, scale=c(2,.4),
            max.words = 50, random.order = FALSE, rot.per = 0.4,
            colors = brewer.pal(8, "Dark2")) 
    }else{
        if (is.null(words2)){
          print("In 'compare' mode you need to specify two set of words")
        }else{
          # Setting plot area
          layout(matrix(c(1,2,3,4), nrow=2), heights=c(1, 4, 1, 4))
          par(mar=rep(0,4))
          
          plot.new()
          text(x=0.5, y=0.3, "First set")
          # Generando nube de palabras para las palabras frecuentes (tweets con retweets)
          wordcloud::wordcloud(words = words1$word, freq= words1$freq, min.freq = 2, scale=c(2,.4),
                    max.words = 50, random.order = FALSE, rot.per = 0.4,
                    colors = brewer.pal(8, "Dark2")) 
          
          plot.new()
          text(x=0.5, y=0.3, "Second set")
          # Generando nube de palabras para las palabras frecuentes (tweets sin retweets)
          wordcloud::wordcloud(words = words2$word, freq= words2$freq, min.freq = 2, scale=c(2,.4),
                    max.words = 50, random.order = FALSE, rot.per = 0.4,
                    colors = brewer.pal(8, "Dark2"))  
        }
    }
  }
  
  if (type=="hist"){
    
    if (compare == FALSE){
      # HISTOGRAMA DE LAS 10 PALABRAS MÁS FRECUENTES
      ggplot2::ggplot(words1[1:10,],aes(words1[,1], words1[,2]))+
        geom_col()+
        title(main="Top 10 frequent words")
      
    }else{
      if (is.null(words2)){
        print("In 'compare' mode you need to specify two set of words")
      }else{
        # Preparando el área para graficar
        # HISTOGRAMA DE LAS 10 PALABRAS MÁS FRECUENTES
        words1 <- words1[1:10,]
        words2 <- words2[1:10,]
        g1 <- ggplot2::ggplot(words1,aes(words1[,1], words1[,2]))+
          geom_col()
        
        g2 <- ggplot2::ggplot(words2,aes(words2[,1], words2[,2]))+
          geom_col()
        
        gridExtra::grid.arrange(g1,g2, nrow=2)
      }
    }
    
  }
}