install.packages('twitteR')
install.packages('ScheduleR')

library(twitteR)
library(httr)
library(SchedulerR) 

# Secret Keys Changed
consumer_key <- "95zTIl1ucMJK3Otm0Az1zQAw3"
consumer_secret <- "rQ6uHq5G8bj660dEvwbzmxUZnq9gMkoemx5m4bAn2How3vnJqM"
access_token <- "791290675-ARVTUes6X89Qr5DLQ8tEJGvElfhjZHXXCOaBh2wm"
access_secret <- "TkFl5fy7wWBBB0Gk1Fe9n2LJdnKjhA18ca4kM8I83U573"

setup_twitter_oauth(consumer_key,consumer_secret,access_token,access_secret)

clinton_tweets <- userTimeline("HillaryClinton", n = 3200)

Hillary_df <- twListToDF(clinton_tweets)
Hillary_df

trump_tweets <- userTimeline("realDonaldTrump", n = 200)
Trump_df <- twListToDF(trump_tweets1)


