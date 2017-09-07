
                                                    ####

#   __________________ #< 59db99f036e24f1b76a754a54d000e29 ># __________________
#   Twitter Analysis Version 1                                              ####



##  .................. #< 83f58384ef7285d34b2856d48361be84 ># ..................
##  Set working Directory                                                   ####

setwd("/Applications/R Data Output")



##  .................. #< a3c813f3c585a77e25fdef1d371ea88d ># ..................
##  Library Setup                                                           ####

suppressPackageStartupMessages({
library(httr)
library(plyr)
library(twitteR)
library(base64enc)
library(RCurl)
library(stringr)
library(wordcloud)
library(tm)
library(igraph)
library(tidyverse)
library(GGally)
library(dplyr)
#library(RWeka) - for two and three word groupings but has java bugs in macos x
})




##  .................. #< f534e04edf7e7347863f78318a67e612 ># ..................
##  twitter authorisations                                                  ####

# Application Settings
# Keep the "Consumer Secret" a secret. This key should never be human-readable in your application.
# Consumer Key (API Key) MDOc987W5tLquqDK2WByXqF4S
# Consumer Secret (API Secret) DA6Wg8jbqAskuUWWKUUO3W32yqaezMroP6XG5w4qzriGQatB2B
# Access Level Read-only (modify app permissions)
# Owner solinos0909
# Owner ID 758938902388355072 


##  .................. #< 3ba7beecf50755206eb169a82787d100 ># ..................
##  Set Twitter Keys                                                        ####

consumerKey <- "MDOc987W5tLquqDK2WByXqF4S"
consumerSecret <- "DA6Wg8jbqAskuUWWKUUO3W32yqaezMroP6XG5w4qzriGQatB2B"
accessToken <- "758938902388355072-GB3Q4WvHzmd4UW8TlOZDrevHSTsN2f6"
accessTokenSecret <- "EXJrsC4LzJm3mx8gZhi3fjlZ1EGNakhCVjAG7W5alMgir"
handle <- "Solinos"

setup_twitter_oauth(consumerKey, consumerSecret, accessToken, accessTokenSecret)



##  .................. #< a278cab56ac1cea720269e7750b0dbc7 ># ..................
##  functions to Analyse Twitter                                            ####

### . . . . . . . . .. #< d682aba977580ebf414eaf3016273545 ># . . . . . . . . ..
### tweet_data                                                              ####

tweet_data <- function(x){
  social_tweets<-unlist(sapply(x,function(t) t$getText()))
  social_isretweet<-unlist(sapply(x,function(t) t$getIsRetweet()))
  social_getretweet<-unlist(sapply(x,function(t) t$getRetweeted()))
  social_retweetcount<-unlist(sapply(x,function(t) t$getRetweetCount()))
  social_screenname<-unlist(sapply(x,function(t) t$getScreenName()))
  social_fav<-unlist(sapply(x,function(t) t$getFavoriteCount()))
  social_retweeters<-unlist(sapply(x,function(t) t$getRetweeters))
  data<-cbind.data.frame(tweet=social_tweets,
                         screenname=social_screenname,
                         retweetcount=social_retweetcount,
                         retweeted=social_isretweet,
                         FavCount=social_fav,
                         stringsAsFactors=FALSE
  )
  return(data)
}


### . . . . . . . . .. #< 07dac378896791d49a6af8904296c35b ># . . . . . . . . ..
### score.sentiment                                                         ####

score.sentiment <- function(sentences, pos.words, neg.words, .progress='none')
{
  # require(plyr)
  # require(stringr)
  scores = laply(sentences, function(sentence, pos.words, neg.words) {
    sentence = gsub('[[:punct:]]', '', sentence)
    sentence = gsub('[[:cntrl:]]', '', sentence)
    sentence = gsub('\\d+', '', sentence)
    sentence = iconv(sentence, "UTF-8", "ASCII")
    tryTolower = function(x){
      y = NA
      try_error = tryCatch(tolower(x), error=function(e) e)
      if (!inherits(try_error, "error"))
        y = tolower(x)
      return(y)
    }
    tweet = sapply(tweet, tryTolower)
    word.list = str_split(sentence, '\\s+')
    words = unlist(word.list)
    pos.matches = match(words, pos.words)
    neg.matches = match(words, neg.words)
    pos.matches = !is.na(pos.matches)
    neg.matches = !is.na(neg.matches)
    score = sum(pos.matches) - sum(neg.matches)
    return(score)
  }, pos.words, neg.words, .progress=.progress )
  scores.df = data.frame(score=scores, text=sentences)
  return(scores.df)
}


##  .................. #< 4f58e0b44b2d2f1878b14c722a18c73c ># ..................
##  Data import                                                             ####

tweetsearch <- c("LGBTI")
social <- searchTwitter(tweetsearch, n = 1500, lang = 'en', since = '2017-01-01')
head(social)

hu.liu.pos  <- scan('positive-words.csv',what='character',comment.char = ';')
hu.liu.neg  <- scan('negative-words.csv',what='character',comment.char = ';')

pos.words <- c(hu.liu.pos,'sweet')
neg.words <- c(hu.liu.neg,'wtf','epicfail')


#Data manipulation
social_tweets <- tweet_data(social)
stats<-social_tweets %>% filter(retweeted == TRUE) %>%
  summarise(tweets=n(),
            n_dist_tweeters=n_distinct(screenname),
            max_retweeet=max(retweetcount),
            mean_retweet=mean(retweetcount),
            sum_retweets=sum(retweetcount),
            median_retweet=median(retweetcount)
  )
social_rt_patterns <- grep("(RT|via)((?:\\b\\W*@\\w+)+)", social_tweets$tweet, ignore.case=TRUE)
social_rt <- social_tweets[social_rt_patterns,]
who_retweet <- as.list(1:length(social_rt_patterns))
who_post <- as.list(1:length(social_rt_patterns))
for (i in 1:length(social_rt_patterns))
{
  twit <- social[[social_rt_patterns[i]]]
  poster = str_extract_all(twit$getText(),
                         "(RT|via)((?:\\b\\W*@\\w+)+)")
  poster=gsub(":","", unlist (poster))
  who_post[[i]] = gsub("(RT @|via @)", "",poster, ignore.case=TRUE)
  who_retweet[[i]] = rep(twit$getScreenName(), length(poster))
}
who_post= unlist(who_post)
who_retweet = unlist(who_retweet)
retweeter_poster = cbind(who_retweet, who_post)
rt_graph = graph.edgelist(retweeter_poster)
ver_lab = get.vertex.attribute(rt_graph, "name", index=V(rt_graph))

#Network mapping
glay <- layout.fruchterman.reingold(rt_graph)
clay <- layout_in_circle(rt_graph)

plot(rt_graph, layout=glay,
     vertex.color=hsv(h=.35,s=1,v=.7,alpha=0.1),
     vertex.frame.color=hsv(h=.35,s=1,v=.7, alpha=0.1),
     vertex.shape="none",
     vertex.size=4,
     vertex.label=ver_lab,
     vertex.label.family="mono",
     vertex.label.color=hsv(h=0, s=0, v=.95, alpha = 0.5),
     vertex.label.cex=.85,
     edge.arrow.size=.4,
     edge.arrow.width=.5,
     edge.width=3,
     edge.color=hsv(h=.35, s=1, v=.7, alpha=0.4))
title(main = "Tweeter Network Map - Fruchterman Reingold", col.main="blue")

plot(rt_graph, layout = clay,
     vertex.color=hsv(h=.35,s=1,v=.7,alpha=0.1),
     vertex.frame.color=hsv(h=.35,s=1,v=.7, alpha=0.1),
     vertex.size = .5,
     vertex.label.color="white",
     vertex.label.distance = 1,
     vertex.label.cex = .75,
     vertex.label = ifelse(igraph::degree(rt_graph) > 25, V(rt_graph)$name, NA),
     edge.color=hsv(h=.35, s=1, v=.7, alpha=0.4),
     edge.width = .5,
     edge.arrow.size=0)
title(main = "Tweeter Network Map - Circle \n degree centrality > 25",
      col.main="blue",
      line = -1.5,
      cex.main = 1.1)



#Network metrics
rt_degree <- degree(rt_graph,mode="total")
rt_between <- betweenness(rt_graph)
rt_eign <- eigen_centrality(rt_graph)
edge_density(rt_graph, loops = TRUE)
rt_g_df <- as.data.frame(rt_between)
rt_g_df$degree <- rt_degree


#Sentiment analysis
score <- score.sentiment(social_tweets$tweet,pos.words,neg.words,.progress='text')
summary(score)
# Histogram of Sentiment Scores
CPCOLS <- "dodgerblue"
ggplot(data = score, aes(x = score)) +
  geom_histogram( bins = 20, fill = CPCOLS, color='yellow') +
  theme_bw() + theme(axis.line = element_line(linetype = "solid"), 
                     axis.ticks = element_line(linetype = "dashed"), 
                     axis.text = element_text(size = 10), 
                     panel.background = element_rect(fill = "antiquewhite"), 
                     plot.background = element_rect(fill = "aliceblue")) +labs(title = "Twitter Sentiment Analysis")

# Histogram Retweets
ggplot(data = social_tweets, aes(x = retweetcount)) +
geom_histogram(bins = 20, fill = CPCOLS, color = 'yellow') +
theme_bw() + theme(panel.background = element_rect(fill = "antiquewhite"), 
    plot.background = element_rect(fill = "aliceblue")) +labs(title = "Twitter ReTweets")



#Wordcloud
social_tweets$tweet <- sapply(social_tweets$tweet,function(row) iconv(row, "latin1", "ASCII", sub=""))
social_corpus <- str_replace_all(social_tweets$tweet,"[^[:graph:]]", " ")
corpus <- Corpus(VectorSource(social_tweets$tweet))
corpus <- corpus %>%
  tm_map(function(x) removeWords(x,stopwords())) %>%
  tm_map(removeWords,c(tweetsearch,"amp","https","t.co", "govhack")) %>%
  tm_map(content_transformer(tolower))
corpus_count <- as.matrix(TermDocumentMatrix(corpus))
corpus_freq <- sort(rowSums(corpus_count), decreasing = TRUE)
col <- brewer.pal(6,"Dark2")
wordcloudname=paste(format(Sys.time(),"%Y-%m-%d %H-%M wordcloud comment"),"jpg",sep=".")
wordcloud(words=names(corpus_freq),freq = corpus_freq, scale = c(5,.75), max.word=100, random.order=F,colors=col)



#output of network statistics#< 69efdb1097957d306e3411b1687df787 >#

data_sc<-cbind(social_tweets,score=score$score,stringsAsFactors=FALSE)
glimpse(data_sc)
