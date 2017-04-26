library(shiny)
# install.packages('twitteR')
# install.packages('ScheduleR')
# install.packages('rsconnect')
library(twitteR)
library(tm)
library(SnowballC)
library(stringr)
library(plyr)
library(tidyverse)
library(wordcloud)
library(plotly)

# Secret Keys Changed
consumer_key <- "95zTIl1ucMJK3Om0Az1zQAw3"
consumer_secret <-
  "rQ2uHq5X8bj9c0dIvwbzmxnq9Mkoemx5m4bAn2How3vnJqM"
access_token <- "791290675-ARVTUes6X895DLQ8tEJGvElfhjZHXCOaBh2wm"
access_secret <- "TkFl5fy7wWBBB0GkGe9n2LJdnKjmA18au4kV8I83U573"

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

CommonWords <- function() {
  wordlist <- file('Stop_words.txt', open = 'r')
  commonwords <- readLines(wordlist)
  close(wordlist)
  commonwords
}


shinyServer(function(input, output, session) {
  TweetDF <- function(searchItem, MaxTerms)
  {
    SampleTweets <-
      searchTwitter(searchItem,
                    lang = "en",
                    n = MaxTerms,
                    resultType = "recent")
    tweets <-
      strip_retweets(SampleTweets,
                     strip_manual = TRUE,
                     strip_mt = TRUE)
    df <- twListToDF(tweets)
    df$text <- sapply(df$text, function(row) iconv(row, "latin1", "ASCII", sub = ""))
    wordcloudentity(df$text)
    return(df)
  }
  
  # Cleaning Tweets, removing links, numbers and symbols.
  cleanTweets <- function(tweets)
  {
    tweets <- gsub("https://t.co/[a-z,A-Z,0-9]*{10}","", tweets)
    tweets <- gsub("[[:punct:]]", "", tweets)
    tweets <- gsub("[[:digit:]]", "", tweets)
    tweets <- str_replace_all(tweets, "#", "")
    tweets <- str_replace_all(tweets,"@[a-z,A-Z]*","")
    
    return(tweets)
  }
  
  wordcloudentity <- function(tweetText)
  {
    tweetCorpus <- Corpus(VectorSource(cleanTweets(tweetText)))
    tweetTDM <- TermDocumentMatrix(tweetCorpus,
                              control = list(
                                              removePunctuation = TRUE,
                                              stopwords = CommonWords(),
                                              removeNumbers = TRUE,
                                              tolower = TRUE
                                              )
      ) 
    tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
    sortedMatrix <-  sort(rowSums(tdMatrix), decreasing = TRUE) 
    cloudFrame <-   data.frame(word = names(sortedMatrix), freq = sortedMatrix)
    wcloudentity <- wordcloud(
                              cloudFrame$word,
                              cloudFrame$freq,
                              min.freq = 5,
                              max.words = 100,
                              colors = brewer.pal(8, "Dark2"),
                              scale = c(7, 0.8),
                              random.color = F,
                              random.order = F
                              )
  }
  
  score.sentiment <- function(sentences, pos.words, neg.words)
  {
    scores <- laply(sentences, function(sentence, pos.words, neg.words) {
      # clean up sentences with R's regex-driven global substitute, gsub():
      sentence <- cleanTweets(sentence)
      
      sentence <- tolower(sentence)
      # split into words. str_split is in the stringr package
      word.list <- str_split(sentence, '\\s+')
      
      words <- unlist(word.list)
      
      # compare our words to the dictionaries
      pos.matches <- match(words, pos.words)
      neg.matches <- match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      pos.matches <- !is.na(pos.matches)
      neg.matches <- !is.na(neg.matches)
      
      score <- sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words)
    scores.df <- data.frame(score = scores,
                           text = sentences,
                           size = seq(length(scores)))
    return(scores.df)
  }
  
  sentimentalanalysis <- function(entity, nooftweets) {
    positivewords <- readLines("positive-words.txt")
    negativewords <- readLines("negative-words.txt")
    entityscore <- score.sentiment(cleanTweets(entity$text), positivewords, negativewords)
    entityscore$posi <- sum(laply(entityscore$score, function(score) {
      if (score > 0) {
        return(1)
      }
      return(0)
    }))
    entityscore$negi <- sum(laply(entityscore$score, function(score) {
      if (score < 0) {
        return(1)
      }
      return(0)
    }))
    entityscore$neu <- nooftweets - (entityscore$posi + entityscore$negi)
    
    entityscore
  }
  
  entity1score <- function() {
    entity1 <- entityone()
    entity1 <- subset(entity1, !is.na(entity1$text))
    scores <- sentimentalanalysis(entity1, nrow(entity1))
    scores
  }
  

  output$entity1wc <- renderText({
    input$entity1
  })
  
  output$entity1wcplot <- renderPlot({
    click()
  })
  
  entityone <- eventReactive(input$go, {
    entityone <- TweetDF(input$entity1, input$maxTweets)
  })
  
  click <- eventReactive(input$go, {
    withProgress(message = 'Getting Tweets',
                 
                 {
                   incProgress(0.5, detail = "50 %")
                   wordcloudentity(entityone()$text)
                 })
  })
  
  
  
  output$plot3 <- renderPlotly({
    withProgress(message = 'Analysing Tweets',
                 detail = "part 0",
                 value = 0,
                 {
                   incProgress(0.5, detail = paste(50, " %"))
                   
                   a <- entity1score()
                   lbl <- c('Positive', 'Negative', 'Neutral')
                   cnt <- c(a$posi[1], a$negi[1], a$neu[1])
                   data <- data.frame(lbl, cnt)
                   p <- plot_ly(
                               data,
                               labels = ~ lbl,
                               values = ~ cnt,
                               type = 'pie',
                               textposition = 'inside',
                               textinfo = 'label+percent',
                               insidetextfont = list(color = '#FFFFFF'),
                               hoverinfo = ~ lbl
                                ) %>%
                        layout(
                               title = 'Tweet Sentiment',
                               xaxis = list(
                                             showgrid = FALSE,
                                             zeroline = FALSE,
                                             showticklabels = FALSE
                                            ),
                               yaxis = list(
                                             showgrid = FALSE,
                                             zeroline = FALSE,
                                             showticklabels = FALSE
                                            )
                              )
                   return(p)
                 })
  })
  
  output$pos <- renderTable({
    a <- entity1score()
    dplyr::filter(a, score > 0) %>% dplyr::select(Tweet = text)
  })
  
  output$neu <- renderTable({
    a <- entity1score()
    dplyr::filter(a, score == 0) %>% dplyr::select(Tweet = text)
  })
  
  output$neg <- renderTable({
    a <- entity1score()
    dplyr::filter(a, score < 0) %>% dplyr::select(Tweet = text)
  })
  
})
