install.packages('base64enc')
install.packages("twitteR")
install.packages("ROAuth")
library(twitteR)
library("ROAuth")
install.packages("plyr")
library(plyr)
install.packages('tm')
library(tm)
install.packages("lazyeval")
library(lazyeval)
install.packages("ggplot2")
library(ggplot2)
install.packages("~/Desktop/Rstem_0.4-1.tar.gz", repos = NULL, type = "source")
install.packages("~/Desktop/sentiment_0.2.tar.gz", repos = NULL, type = "source")
library(Rstem)
library(sentiment)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("wordcloud")
library(wordcloud)
consumerKey="YourConsumerKey"
consumerSecret="YourConsumerSecretKey"
access_token="YourAccessToken"
access_secret="YourAccessSecret"
requestURL="https://api.twitter.com/oauth/request_token"
authURL="https://api.twitter.com/oauth/authorize"
accessURL="https://api.twitter.com/oauth/access_token"
twitCred <- OAuthFactory$new(consumerKey=consumerKey,consumerSecret=consumerSecret,requestURL=requestURL,accessURL=accessURL,authURL=authURL)
#download.file(url="http://curl.haxx.se/ca/cacert.pem", destfile="cacert.pem")
#twitCred$handshake(cainfo="cacert.pem")
save(list="twitCred", file="twitteR_credentials")
load("twitteR_credentials")
setwd("SetTheWorkingDirectoryToStoreTheCSVfile")
setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_secret)
setup_twitter_oauth(consumerKey, consumerSecret, access_token, access_secret)
MyTweets <-searchTwitter("#ofYourChoice", n=10000, lang="en")
MyTweets.df <- twListToDF(MyTweets)
dim(MyTweets.df)
View(MyTweets.df)
write.csv(MyTweets.df, file=" MyTweets.csv", row.names=F)
mytweets <- sapply(MyTweets, function(x) x$getText())
mytweets = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", "", mytweets)
mytweets = gsub("@\\w+", "", mytweets)
mytweets = gsub("[[:punct:]]", "", mytweets)
mytweets = gsub("[[:digit:]]", "", mytweets)
mytweets = gsub("http\\w+", "", mytweets)
mytweets = gsub("[ \t]{2,}", "", mytweets)
mytweets = gsub("^\\s+|\\s+$", "", mytweets)
try.error = function(x)
{
      y = NA
      try_error = tryCatch(tolower(x), error=function(e) e)
      if (!inherits(try_error, "error"))
            y = tolower(x)
      return(y)
}
mytweets = sapply(mytweets, try.error)
mytweets = mytweets[!is.na(mytweets)]
names(mytweets) = NULL
class_emo = classify_emotion(mytweets, algorithm="bayes", prior=1.0)
emotion = class_emo[,7]
emotion[is.na(emotion)] = "unknown"
class_pol = classify_polarity(mytweets, algorithm="bayes")
polarity = class_pol[,4]
sent_df = data.frame(text=mytweets, emotion=emotion,polarity=polarity, stringsAsFactors=FALSE)
sent_df = within(sent_df,emotion <- factor(emotion, levels=names(sort(table(emotion), decreasing=TRUE))))
write.csv(sent_df, file=" SWA.csv", row.names=F)
ggplot(sent_df, aes(x=emotion)) + geom_bar(aes(y=..count.., fill=emotion)) +
      scale_fill_brewer(palette="Dark2") +
      ggtitle('Sentiment Analysis of Tweets on Hastag') +
      theme(legend.position='right') + ylab('Number of Tweets') + xlab('Emotion Categories')
ggplot(sent_df, aes(x=polarity)) +
      geom_bar(aes(y=..count.., fill=polarity)) +
      scale_fill_brewer(palette="RdGy") +
      ggtitle('Polarity Analysis of Tweets on Hastag ') +
      theme(legend.position='right') + ylab('Number of Tweets') + xlab('Polarity Categories')


#code for emotion word clouds
airlines_emos = levels(factor(sent_df$emotion))
n_airlines_emos = length(airlines_emos)
airlines.emo.docs = rep("", n_airlines_emos)
for (i in 1:n_airlines_emos)
{
      tmp = mytweets[emotion == airlines_emos[i]]
      airlines.emo.docs[i] = paste(tmp, collapse="")
}
airlines.emo.docs = removeWords(airlines.emo.docs, stopwords("english"))
airlines.corpus = Corpus(VectorSource(airlines.emo.docs))
airlines.tdm = TermDocumentMatrix(airlines.corpus)
airlines.tdm = as.matrix(airlines.tdm)
colnames(airlines.tdm) = airlines_emos
comparison.cloud(airlines.tdm, colors = brewer.pal(n_airlines_emos, "Dark2"),scale = c(1,.5), random.order = FALSE, title.size = 1)


