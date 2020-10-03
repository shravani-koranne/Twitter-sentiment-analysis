library(twitteR)
library(openssl)
library(httpuv)
library(base64enc)
library(ROAuth)
library(RCurl)
library(httr)

library(stringr)
library(plyr)
library(dplyr)
library(tm)

#library(ggmap)
#library(wordcloud)

key<-"blJYbLom9c0S9oUeMXwNDO621" 
skey<-"k674UCYKNZQYIDD4Vlrw8QQKQJldj9hpF7yPGgSzo14aEHNdV4"

atoken<-"1134747843250417665-Gv1tB5BUB5DlSN3BildXCf4eeb8531"
astoken<-"zxXLwDuLHRGsqTs9RsF0Xr3SiAYPTDq6IKgWlmvF8UOiM"

setup_twitter_oauth(key,skey,atoken,astoken)
tweet = searchTwitter("swiggy", n=5000, 
                       lang="en", 
                       geocode="28.704060,77.102493,150mi")
tweettext = sapply(tweet, function(x) x$getText())

tweetdate=lapply(tweet, function(x) x$getCreated())

tweetdate1=sapply(tweetdate,function(x) strftime(x,format ="%Y-%m-%d %H:%M:%S", tz="UTC"))

tweettext=lapply(tweettext,function(x) iconv(x,"latin1","ASCII",sub = ""))
tweettext=lapply(tweettext, function(x) gsub("htt.*","",x))
tweettext=lapply(tweettext,function(x) gsub("#","",x))
tweettext=lapply(tweettext,function(x) gsub("@","",x))
tweettext=unlist(tweettext)

pos=readLines("positive-words.txt")
neg=readLines("negative-words.txt")
neg2=c(neg,"looting","no","never","cancelled","disrupting","barely","wasted","waste","worst","frauds","disappointing ");
tail(neg2)

sentimentfun = function(tweettext, pos, neg2, .progress='non')
{
  # Parameters
  # tweettext: vector of text to score
  # pos: vector of words of postive sentiment
  # neg: vector of words of negative sentiment
  # .progress: passed to laply() 4 control of progress bar
  
  # create simple array of scores with laply
  scores = laply(tweettext,
                 function(singletweet, pos, neg2)
                 {
                   # remove punctuation - using global substitute
                   singletweet = gsub("[[:punct:]]", "", singletweet)
                   # remove control characters
                   singletweet = gsub("[[:cntrl:]]", "", singletweet)
                   # remove digits
                   singletweet = gsub("\\d+", "", singletweet)
                 
                   # define error handling function when trying tolower
                   tryTolower = function(x)
                   {
                     # create missing value
                     y = NA
                     # tryCatch error
                     try_error = tryCatch(tolower(x), error=function(e) e)
                     # if not an error
                     if (!inherits(try_error, "error"))
                       y = tolower(x)
                     # result
                     return(y)
                   }
                   # use tryTolower with sapply 
                   singletweet = sapply(singletweet, tryTolower)
                   
                   # split sentence into words with str_split (stringr package)
                   word.list = str_split(singletweet, "\\s+")
                   words = unlist(word.list)
                   
                   # compare words to the dictionaries of positive & negative terms
                   pos.matches = match(words, pos)
                   neg2.matches = match(words, neg2)
                   
                   # get the position of the matched term or NA
                   # we just want a TRUE/FALSE
                   pos.matches = !is.na(pos.matches)
                   neg2.matches = !is.na(neg2.matches)
                   
                   # final score
                   score = sum(pos.matches) - sum(neg2.matches)
                   return(score)
                 }, pos, neg2, .progress=.progress )
  
  # data frame with scores for each sentence
  sentiment.df = data.frame(text=tweettext, score=scores)
  View(sentiment.df)
  return(sentiment.df)
}
isretweet=sapply(tweet, function(x) x$getIsRetweet())

retweetcount=sapply(tweet, function(x) x$getRetweetCount())

favoritecount=sapply(tweet, function(x) x$getFavoriteCount())
scores=sentimentfun(tweettext,pos,neg2,.progress = "text")
totalscores=sum(scores$score)

## Creating the Data Frame
data=as.data.frame(cbind(ttext=tweettext,
                         date=tweetdate1,
                         isretweet=isretweet, 
                         retweetcount=retweetcount,
                         favoritecount=favoritecount,
                         score=scores$score,
                         delivery= "swiggy",location="mumbai" ))
View(data)

## remove duplicates
data2 = duplicated(data[,1])

data$duplicate = data2
## create file to wd
write.csv(data, file= "swiggy_mumbai.csv")