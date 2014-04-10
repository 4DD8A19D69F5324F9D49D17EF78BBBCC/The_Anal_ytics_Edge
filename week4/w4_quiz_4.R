library(rpart.plot)
library(rpart)
library(ROCR)
library(caTools)
library(randomForest)
library(caret)
library(e1071)
library(class)
library(ggplot2)
acc = function(pred,real){
  sum(pred==real) / length(real)
}
census = read.csv('Downloads/census.csv')

levels(census$over50k) = c(FALSE,TRUE)



set.seed(2000)
split = sample.split(census$over50k,SplitRatio=0.6)
Train = census[split,]
Test = census[!split,]

Logmodel = glm(over50k ~ .,data=Train,family=binomial)
summary(Logmodel)


acc(predict(Logmodel,newdata=Test,type='response')>0.5,Test$over50k)
pred = prediction(predict(Logmodel,newdata=Test,type='response'),Test$over50k)
performance(pred,'auc')
plot(performance(pred,'tpr','fpr'))

Cartmodel = rpart(over50k ~ .,data = Train,method='class')
prp(Cartmodel)
acc(predict(Cartmodel,newdata=Test,type='class'),Test$over50k)

predcart = prediction(predict(Cartmodel,newdata=Test)[,2],Test$over50k)
plot(performance(predcart,'tpr','fpr'))


set.seed(1)
TrainSmall = Train[sample(nrow(Train), 2000), ]
set.seed(1)
TrainSmall$over50k = as.factor(TrainSmall$over50k)
Test$over50k = as.factor(Test$over50k)
rfmodel = randomForest(over50k ~ . - nativecountry,data=TrainSmall)
acc(predict(rfmodel,newdata=Test),Test$over50k)

vu = varUsed(rfmodel, count=TRUE)

vusorted = sort(vu, decreasing = FALSE, index.return = TRUE)

dotchart(vusorted$x, names(rfmodel$forest$xlevels[vusorted$ix]))
varImpPlot(rfmodel)


cartGrid = expand.grid( .cp = seq(0.002,0.1,0.002))
tctrl = trainControl(method='cv',number=10)
set.seed(2)
train(over50k ~ .,data=Train,method='rpart',trControl=tctrl,tuneGrid=cartGrid)


Cartmodel2 = rpart(over50k ~ . ,data= Train,method='class',cp=0.002)
acc(predict(Cartmodel2,Test,type='class'),Test$over50k)
