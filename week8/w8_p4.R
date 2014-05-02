library(tm)
library(wordcloud)
library(RColorBrewer)

tweets = read.csv('tweets.csv')

#tweets = subset(tweets,Avg<=-1)
corpus = Corpus(VectorSource(tweets))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords,c(stopwords('english'),'apple'))
dtm = DocumentTermMatrix(corpus)
allTweets = as.data.frame(as.matrix(dtm))


wordcloud(colnames(allTweets),colSums(allTweets),scale=c(2, 0.5),random.order=F)
  