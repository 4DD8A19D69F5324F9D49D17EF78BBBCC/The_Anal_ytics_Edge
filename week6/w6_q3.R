re = read.csv('reimbursement.csv')
re2 = re[-c(1,13,14)]
sum(rowSums(re2)!=0) / nrow(re2)
tmp=cor(re)
for(i in 1:14){
  tmp[i,i]=0
}
max(tmp)

re$reimbursement2008 = log(re$reimbursement2008+1)
re$reimbursement2009 = log(re$reimbursement2009+1)

set.seed(144)
spl = sample(1:nrow(re), size=0.7*nrow(re))
train = re[spl,]
test = re[-spl,]
lm.re = lm(reimbursement2009 ~ .,data=train)
summary(lm.re)

pred = predict(lm.re,newdata=test)
sqrt(mean((pred-test$reimbursement2009)^2))
sqrt(mean(( mean(train$reimbursement2009)-test$reimbursement2009)^2))
sqrt(mean(( test$reimbursement2008-test$reimbursement2009)^2))

train.limited = train
train.limited$reimbursement2009 = NULL
test.limited = test
test.limited$reimbursement2009 = NULL

library(caret)
preproc = preProcess(train.limited)
train.norm = predict(preproc, train.limited)
test.norm = predict(preproc, test.limited)


sapply(train.norm,mean)
sapply(test.norm,mean)

set.seed(144)
km = kmeans(train.norm,3)

km$centers

install.packages('flexclust')
library(flexclust)
km.kcca = as.kcca(km, train.norm)
cluster.train = predict(km.kcca)
cluster.test = predict(km.kcca, newdata=test.norm)
tapply(train$reimbursement2009,cluster.train,mean)


trains = list()

for(i in 1:3){
  trains[[i]] = train[cluster.train==i,]
}

lms = list()

for(i in 1:3){
  lms[[i]] = lm(reimbursement2009 ~ . ,data=trains[[i]])
}


pred.test = list()

for(i in 1:3){
  pred.test[[i]]=predict(lms[[i]],newdata=test[cluster.test==i,])
}
lapply(pred.test,FUN=mean)

lapply(pred.test,FUN=function(pred){ sqrt(mean((pred-test[cluster.test==i,]$reimbursement2009)^2))})
library(plyr)

all.predictions = c(pred.test[[1]], pred.test[[2]], pred.test[[3]])
all.outcomes = c(test[cluster.test==1,]$reimbursement2009, test[cluster.test==2,]$reimbursement2009, test[cluster.test==3,]$reimbursement2009)

sqrt(mean((all.predictions-all.outcomes)^2))
