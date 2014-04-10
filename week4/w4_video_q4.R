library(rpart.plot)
library(rpart)
library(ROCR)
library(caTools)
library(randomForest)
library(caret)
library(e1071)
library(class)
library(ggplot2)
stevens = read.csv('Downloads/stevens.csv')
set.seed(3000)
split = sample.split(stevens$Reverse,SplitRatio=0.7)
Train = stevens[split,]
Test = stevens[!split,]

StevensTree = rpart(Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt+ Unconst,data=Train,method='class',control=rpart.control(minbucket=25))
pd = predict(StevensTree,newdata=Test)
pred = prediction(pd[,2],Test$Reverse)

param = Reverse ~ Circuit + Issue + Petitioner + Respondent + LowerCourt+ Unconst

print(as.numeric(performance(pred, "auc")@y.values))
StevensTree2 = rpart(param,data=Train,method='class',control=rpart.control(minbucket=5))
StevensTree3 = rpart(param,data=Train,method='class',control=rpart.control(minbucket=100))


Train$Reverse = as.factor(Train$Reverse)
Test$Reverse = as.factor(Test$Reverse)

set.seed(100)
StevensForest = randomForest(param,data=Train,ntree=200,maxnodes=25)

tbl = table(Test$Reverse,predict(StevensForest,newdata=Test))

acc = (tbl[1,1] + tbl[2,2])/sum(tbl)

print(acc)


fitControl = trainControl(method='cv',number=10)
cartGrid = expand.grid(.cp=(1:50)*0.01)
#train(param,data=Train,method='rpart',trControl=fitControl,tuneGrid=cartGrid)
StevensTreeCV = rpart(param,data=Train,method='class',control=rpart.control(cp=0.19))
