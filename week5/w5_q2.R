library(tm)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caTools)

corpus.make.dtm = function(src){
  corpus = Corpus(VectorSource(src))
  corpus = tm_map(corpus,tolower)
  corpus = tm_map(corpus,removePunctuation)
  corpus = tm_map(corpus,removeWords,stopwords('english'))
  corpus = tm_map(corpus,stemDocument)
  dtm = DocumentTermMatrix(corpus)
}

trials=read.csv("Downloads/clinical_trial.csv",stringsAsFactors=FALSE)
abstract.length = nchar(trials$abstract,type='byte')
sum(abstract.length==0)
trials$title[which.min(nchar(trials$title))]

dtmTitle = corpus.make.dtm(trials$title)
dtmAbstract = corpus.make.dtm(trials$abstract)

sparseTitle = removeSparseTerms(dtmTitle,0.95)
sparseAbstract = removeSparseTerms(dtmAbstract,0.95)

dfTitle = as.data.frame(as.matrix(sparseTitle))
dfAbstract = as.data.frame(as.matrix(sparseAbstract))

colnames(dfTitle) = paste0("T", colnames(dfTitle))
colnames(dfAbstract) = paste0("A", colnames(dfAbstract))


dtm = cbind(dfTitle,dfAbstract)
dtm$trial = trials$trial


set.seed(144)
spt = sample.split(dtm$trial,SplitRatio=0.7)

train = dtm[spt,]
test = dtm[!spt,]



trialCART = rpart(trial ~ . , data = train, method ='class')
prp(trialCART)
max(predict(trialCART)[,2])
max(predict(trialCART,newdata=test)[,2])


pred = predict(trialCART)[,2]>0.5

tbl=table(pred,train$trial)

accu = (tbl[1,1]+tbl[2,2])/sum(tbl)
sens = tbl[2,2]/(tbl[1,2]+tbl[2,2])
spec = tbl[1,1]/(tbl[1,1]+tbl[2,1])


predtest = predict(trialCART,newdata=test)[,2]
table(predtest>0.5,test$trial)
pd = prediction(predtest,test$trial)

performance(pd,'auc')@y.values
