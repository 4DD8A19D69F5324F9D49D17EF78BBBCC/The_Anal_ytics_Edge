wiki = read.csv('Downloads/wiki.csv',stringsAsFactors=FALSE)
wiki$Vandal = as.factor(wiki$Vandal)

table(wiki$Vandal)

library(tm)


corpus.make.dtm = function(src){
  corpus = Corpus(VectorSource(src))
  corpus = tm_map(corpus,removeWords,stopwords('english'))
  corpus = tm_map(corpus,stemDocument)
  dtm = DocumentTermMatrix(corpus)
}

corpus.make.data.frame = function(src){
  dtm = corpus.make.dtm(src)
  sparse = removeSparseTerms(dtm,0.997)
  as.data.frame(as.matrix(sparse))
}

wordsAdded = corpus.make.data.frame(wiki$Added)
wordsRemoved = corpus.make.data.frame(wiki$Removed)

colnames(wordsAdded) = paste("A", colnames(wordsAdded))
colnames(wordsRemoved) = paste("R", colnames(wordsRemoved))

wikiWords = cbind(wordsAdded,wordsRemoved)
wikiWords$Vandal = wiki$Vandal

library(caTools)

set.seed(123)
spt = sample.split(wikiWords$Vandal,SplitRatio=0.7)
train = wikiWords[spt,]
test = wikiWords[!spt,]
table(test$Vandal)
618/ (618+545)

library(rpart)
library(rpart.plot)
CARTmodel = rpart(Vandal ~ ., data=train,method='class')

pred = predict(CARTmodel,newdata=test,type='class')

table(test$Vandal,pred)

(618+12) / (618+533+12)



wikiWords2 = wikiWords
wikiWords2$HTTP = ifelse(grepl("http",wiki$Added,fixed=TRUE), 1, 0)
table(wikiWords2$HTTP)

train2 = wikiWords2[spt,]
test2 = wikiWords2[!spt,]

CARTmodel2 = rpart(Vandal ~ . ,data= train2, method ='class')
pred2 = predict(CARTmodel2,newdata=test2,type='class')
table(test2$Vandal,pred2)

(609+57)/ (609+9+488+57)


dtmAdded = corpus.make.dtm(wiki$Added)
dtmRemoved = corpus.make.dtm(wiki$Removed)


wikiWords2$NumWordsAdded = rowSums(as.matrix(dtmAdded))
wikiWords2$NumWordsRemoved = rowSums(as.matrix(dtmRemoved))


train3 = wikiWords2[spt,]
test3 = wikiWords2[!spt,]


CARTmodel3 = rpart(Vandal ~ . ,data= train3, method ='class')
pred3 = predict(CARTmodel3,newdata=test3,type='class')
table(test3$Vandal,pred3)

(514+248) / nrow(test3)

wikiWords3 = wikiWords2
wikiWords3$Minor = wiki$Minor
wikiWords3$Loggedin = wiki$Loggedin


train4 = wikiWords3[spt,]
test4 = wikiWords3[!spt,]
CARTmodel4 = rpart(Vandal ~ . ,data= train4, method ='class')
pred4 = predict(CARTmodel4,newdata=test4,type='class')
table(test4$Vandal,pred4)

(595+241) / nrow(test4)

prp(CARTmodel4)
