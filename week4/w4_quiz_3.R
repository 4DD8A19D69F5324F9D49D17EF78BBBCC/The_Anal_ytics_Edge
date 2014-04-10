library(rpart.plot)
library(rpart)
library(ROCR)
library(caTools)
library(randomForest)
library(caret)
library(e1071)
library(class)
library(ggplot2)

data(state)
statedata = data.frame(state.x77)

sse <- function(pred,real){
  sum((pred-real)^2)
}

linearmodel = lm(Life.Exp ~ .,data=statedata)
summary(linearmodel)
sse(predict(linearmodel),statedata$Life.Exp)
linearmodel2 = lm(Life.Exp ~ Population + Murder + Frost + HS.Grad,data=statedata)
summary(linearmodel2)
sse(predict(linearmodel2),statedata$Life.Exp)
cartmodel = rpart(Life.Exp ~ .,data=statedata)
prp(cartmodel)
sse(predict(cartmodel),statedata$Life.Exp)
cartmodel2 = rpart(Life.Exp ~ .,data=statedata,control=rpart.control(minbucket=5))
prp(cartmodel2)

sse(predict(cartmodel2),statedata$Life.Exp)


cartmodel3 = rpart(Life.Exp ~ Area,data=statedata,control=rpart.control(minbucket=1))

sse(predict(cartmodel3),statedata$Life.Exp)
set.seed(111)

fitControl = trainControl(method='cv',number=10)
cartGrid = expand.grid(.cp=(1:50)*0.01)
train(Life.Exp ~ . ,data=statedata,method='rpart',trControl=fitControl,tuneGrid=cartGrid)

cartmodel4 = rpart(Life.Exp ~ .,data=statedata,cp=0.12)
prp(cartmodel4)
sse(predict(cartmodel4),statedata$Life.Exp)

train(Life.Exp ~ Area ,data=statedata,method='rpart',trControl=fitControl,tuneGrid=cartGrid)

cartmodel5 = rpart(Life.Exp ~ Area,data=statedata,cp=0.06)
prp(cartmodel5)
sse(predict(cartmodel5),statedata$Life.Exp)
