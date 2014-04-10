library(tm)
library(rpart)
library(rpart.plot)
library(ROCR)
library(caTools)
library(randomForest)
library(slam)

corpus.make.dtm = function(src){
  corpus = Corpus(VectorSource(src))
  corpus = tm_map(corpus,tolower)
  corpus = tm_map(corpus,removePunctuation)
  corpus = tm_map(corpus,removeWords,stopwords('english'))
  corpus = tm_map(corpus,stemDocument)
  dtm = DocumentTermMatrix(corpus)
}

emails = read.csv("Downloads/emails.csv",stringsAsFactors =FALSE)
sum(emails$spam)

max(nchar(emails$text))
which.min(nchar(emails$text))

dtm = corpus.make.dtm(emails$text)
spdtm = removeSparseTerms(dtm,0.95)


emailsSparse = as.data.frame(as.matrix(spdtm))
colnames(emailsSparse) = make.names(colnames(emailsSparse))

which.max(colSums(emailsSparse))
emailsSparse$spam = emails$spam
freqs = colSums(subset(emailsSparse,spam==1))
sum(freqs>1000)

emailsSparse$spam = as.factor(emailsSparse$spam)

set.seed(123)
spt = sample.split(emailsSparse$spam,SplitRatio=0.7)
train = emailsSparse[spt,]
test = emailsSparse[!spt,]


spamLog = glm(spam ~ .,data=train,family=binomial)
spamCART = rpart(spam ~ .,data= train, method = 'class')
set.seed(123)
spamRF = randomForest(spam ~ . ,data= train)


performance(prediction(predict(spamCART)[,2],train$spam),'auc')
performance(prediction(predict(spamRF,type='prob')[,2],train$spam),'auc')


predLog = predict(spamLog,newdata=test,type='response')
predCART = predict(spamCART,newdata=test,type='prob')[,2]
predRF = predict(spamRF,newdata=test,type='prob')[,2]

table(predLog>0.5,test$spam)
performance(prediction(predLog,test$spam),'auc')@y.values

table(predCART>0.5,test$spam)
performance(prediction(predCART,test$spam),'auc')@y.values


table(predRF>0.5,test$spam)
performance(prediction(predRF,test$spam),'auc')@y.values


library(slam)

wordCount = rollup(dtm, 2, FUN=sum)$v

hist(wordCount)
hist(log(wordCount))

emailsSparse$logWordCount = log(wordCount)
boxplot(emailsSparse$logWordCount ~ emailsSparse$spam)


train2 = emailsSparse[spt,]
test2 = emailsSparse[!spt,]

spamCART2 = rpart(spam ~ . ,data = train2,method = 'class')
spamRF2 = randomForest(spam ~ . ,data = train2)
prp(spamCART2)


predCART2 = predict(spamCART2,newdata=test2,type='prob')[,2]
predRF2 = predict(spamRF2,newdata=test2,type='prob')[,2]

table(predCART2>0.5,test$spam)
performance(prediction(predCART2,test$spam),'auc')@y.values

table(predRF2>0.5,test$spam)
performance(prediction(predRF2,test$spam),'auc')@y.values

corpus = Corpus(VectorSource(emails$text))
corpus = tm_map(corpus,tolower)
corpus = tm_map(corpus,removePunctuation)
corpus = tm_map(corpus,removeWords,stopwords('english'))
corpus = tm_map(corpus,stemDocument)
install.packages('RTextTools')
library(RTextTools)

dtm2gram = create_matrix(as.character(corpus), ngramLength=2)

spdtm2gram = removeSparseTerms(dtm2gram,sparse=0.95)
emailSparse2gram =  as.data.frame(as.matrix(spdtm2gram))
colnames(emailSparse2gram) = make.names(colnames(emailSparse2gram))
emailsCombined = cbind(emailsSparse,emailSparse2gram)

train3 = emailsCombined[spt,]
test3 = emailsCombined[!spt,]

spamCARTcombined = rpart(spam ~ . ,data= train3)
set.seed(123)
spamRFcombined = randomForest(spam ~ . ,data = train3)

prp(spamCARTcombined,varlen=0)


table(predict(spamCARTcombined,newdata=test3)[,2]>0.5,test3$spam)
performance(prediction(predict(spamCARTcombined,newdata=test3)[,2],test3$spam),'auc')@y.values

table(predict(spamRFcombined,newdata=test3,type='prob')[,2]>0.5,test3$spam)
performance(prediction(predict(spamRFcombined,newdata=test3,type='prob')[,2],test3$spam),'auc')@y.values

