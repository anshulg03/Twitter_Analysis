# install.packages('twitteR')
# install.packages('ScheduleR')

library(twitteR)
library(tm)
library(SnowballC) 
library(stringr)
library(plyr)

# Secret Keys Changed
consumer_key <- "95zTIl1ucMJK3Otm0Az1zQAw3"
consumer_secret <- "rQ6uHq5G8bj660dEvwbzmxUZnq9gMkoemx5m4bAn2How3vnJqM"
access_token <- "791290675-ARVTUes6X89Qr5DLQ8tEJGvElfhjZHXXCOaBh2wm"
access_secret <- "TkFl5fy7wWBBB0Gk1Fe9n2LJdnKjhA18ca4kM8I83U573"

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)


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

<<<<<<< HEAD

# corpus = Corpus(VectorSource(df$text))
# corpus <- tm_map(corpus, removeURL)
# corpus = tm_map(corpus, tolower)
# corpus = tm_map(corpus, removePunctuation)
# corpus = tm_map(corpus, removeWords, c("apple", stopwords("english")))
# corpus = tm_map(corpus, stemDocument)
# frequencies = DocumentTermMatrix(corpus)

# sparse = removeSparseTerms(frequencies, 0.995)
# tweetsSparse = as_tibble(as.matrix(sparse))
# colnames(tweetsSparse) = make.names(colnames(tweetsSparse))
  
wordcloudentity<-function(tweetText)
{
  tweetCorpus<-Corpus(VectorSource(cleanTweets(tweetText)))
  #tweetCorpus2<- tm_map(tweetCorpus, stemDocument, language = "english",lazy=TRUE)  
  tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                        stopwords=stopwords("english"),
                                                        removeNumbers=TRUE,tolower=TRUE))
  tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
  sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
  cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
  wcloudentity<-wordcloud(cloudFrame$word,cloudFrame$freq,max.words=100, colors=brewer.pal(8,"Dark2"),scale=c(4,.5), random.order=F)
}

score.sentiment = function(sentences, pos.words, neg.words)
{
  
  # we got a vector of sentences. plyr will handle a list
  # or a vector as an "l" for us
  # we want a simple array ("a") of scores back, so we use 
  # "l" + "a" + "ply" = "laply":
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
  
  positivewords=readLines("positive_words.txt")
  negativewords=readLines("negative_words.txt")
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


df <- TweetDF('@realDonaldTrump','200')
df <- subset(df,!is.na(df$text))
snt <- sentimentalanalysis(df,nrow(df))
=======
# WIP
>>>>>>> origin/master
