library(shiny)
# install.packages('twitteR')
# install.packages('ScheduleR')
# install.packages('rsconnect')
library(twitteR)
library(tm)
library(SnowballC) 
library(stringr)
library(plyr)
library(rsconnect)
library(wordcloud)
library(plotly)

# Secret Keys Changed
consumer_key <- "95zTIl1ucMJK3Otm0Az1zQAw3"
consumer_secret <- "rQ2uHq5X8bj9c0dIvwbzmxUZnq9gMkoemx5m4bAn2How3vnJqM"
access_token <- "791290675-ARVTUes6X89Qr5DLQ8tEJGvElfhjZHXXCOaBh2wm"
access_secret <- "TkFl5fy7wWBBB0Gk1Ge9n2LJdnKjmA18au4kV8I83U573"

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)
# Define server logic required to draw a histogram
Commonwords <- function(){
  wordlist <-file('Stop_words.txt',open='r')
  commonwords <- readLines(wordlist)
  close(wordlist)
  commonwords
}


shinyServer(function(input, output, session) {
   
  observeEvent(input$go,{
    withProgress(message = 'Creating plot', style = 'notification', value = 0.1, {
      Sys.sleep(0.25)
      
      # Create 0-row data frame which will be used to store data
      dat <- data.frame(x = numeric(0), y = numeric(0))
      
      # withProgress calls can be nested, in which case the nested text appears
      # below, and a second bar is shown.
      withProgress(message = 'Generating data', detail = "part 0", value = 0, {
        for (i in 1:100) {
          # Each time through the loop, add another row of data. This a stand-in
          # for a long-running computation.
          dat <- rbind(dat, data.frame(x = rnorm(1), y = rnorm(1)))
          
          # Increment the progress bar, and update the detail text.
          incProgress(0.1, detail = paste(i, " %"))
          
          # Pause for 0.1 seconds to simulate a long computation.
          Sys.sleep(0.05)
        }
      })
    })
  })
  
  
  
  TweetDF <- function(searchItem, MaxTerms)
  {
    SampleTweets <- searchTwitter(searchItem, lang="en", n=MaxTerms, resultType="recent")
    tweets <-  strip_retweets(SampleTweets, strip_manual = TRUE, strip_mt = TRUE)
    df <- twListToDF(tweets)
    df$text <- sapply(df$text,function(row) iconv(row, "latin1", "ASCII", sub=""))
    wordcloudentity(df$text)
    return(df)
  }
  
  
  cleanTweets <- function(tweets)
  {
    tweets <- gsub("http[[:alnum:]]*", "", tweets)
    tweets <- gsub("[[:punct:]]", "", tweets)
    tweets <- gsub("[[:digit:]]", "", tweets)
    tweets <- str_replace_all(tweets,"#","")
    tweets <- str_replace_all(tweets,"@","")
    return(tweets)
  }
  
  wordcloudentity<-function(tweetText)
  {
    tweetCorpus<-Corpus(VectorSource(cleanTweets(tweetText)))
    tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                          stopwords=Commonwords(),
                                                          removeNumbers=TRUE,tolower=TRUE))
    tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
    sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
    cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
    wcloudentity<-wordcloud(cloudFrame$word,cloudFrame$freq,max.words=100, colors=brewer.pal(8,"Dark2"),scale=c(8,1), random.order=F)
  }
  
  score.sentiment = function(sentences, pos.words, neg.words)
  {
    
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      # clean up sentences with R's regex-driven global substitute, gsub():
      sentence = cleanTweets(sentence)
      # and convert to lower case:
      sentence = tolower(sentence)
      # split into words. str_split is in the stringr package
      word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
      words = unlist(word.list)
      
      
      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words)
    scores.df = data.frame(score=scores, text=sentences, size=seq(length(scores)))
    return(scores.df)
  }
  
  sentimentalanalysis<-function(entity,nooftweets){
    
    positivewords=readLines("positive-words.txt")
    negativewords=readLines("negative-words.txt")
    entityscore = score.sentiment(cleanTweets(entity$text),positivewords,negativewords)
    entityscore$posi = sum(laply(entityscore$score,function(score){
      if(score>0){
        return(1)
      }
      return(0)}))
    entityscore$negi = sum(laply(entityscore$score,function(score){
      if(score<0){
        return(1)
      }
      return(0)}))
    entityscore$neu = nooftweets-(entityscore$posi+entityscore$negi)
    
    entityscore
  }   
  
  entity1score <-function(){
    entity1<-entityone()
    entity1<-subset(entity1,!is.na(entity1$text))
    scores <-sentimentalanalysis(entity1,nrow(entity1))
    scores
  }
  
  
  
  output$entity1wc<-renderText({
    
    input$entity1})
  
  output$entity1wcplot<-renderPlot({
    click()})
  
  entityone<-reactive({
    entityone<-TweetDF(input$entity1, input$maxTweets)
  })

  click <- eventReactive(input$go,{
        wordcloudentity(entityone()$text)
  })
  

    
    output$plot3<- renderPlotly({
      
      a<-entity1score()
      lbl <- c('Positive','Negative','Neutral')
      cnt <- c(a$posi[1],a$negi[1],a$neu[1])
      data <- data.frame(lbl,cnt)
      p <- plot_ly(data, labels = ~lbl, values = ~cnt, type = 'pie',textposition = 'inside',
                   textinfo = 'label+percent',
                   insidetextfont = list(color = '#FFFFFF'),
                   hoverinfo = ~lbl) %>%
               layout(title = 'Tweet Sentiment',
               xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
               yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
                
      return(p)
      })
    
  })
  
