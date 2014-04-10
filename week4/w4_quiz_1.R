library(rpart.plot)
library(rpart)
library(ROCR)
library(caTools)
library(randomForest)
library(caret)
library(e1071)
library(class)
library(ggplot2)

gerber  = read.csv('Downloads/gerber.csv')

print(sum(gerber$voting) / nrow(gerber))

cor(gerber$civicduty,gerber$voting)
cor(gerber$hawthorne,gerber$voting)
cor(gerber$self,gerber$voting)
cor(gerber$neighbors,gerber$voting)

logmodel = glm(voting ~ hawthorne + civicduty + neighbors + self,data=gerber,family=binomial)
summary(logmodel)

logpd = predict(logmodel,type='response')
sum((logpd>0.3) == gerber$voting)/nrow(gerber)
sum((logpd>0.5) == gerber$voting)/nrow(gerber)
pred = prediction(logpd,gerber$voting)
performance(pred,"auc")

CARTmodel = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber)
prp(CARTmodel)
CARTmodel2 = rpart(voting ~ civicduty + hawthorne + self + neighbors, data=gerber, cp=0.0)
prp(CARTmodel2)
CARTmodel3 = rpart(voting ~ control+civicduty + hawthorne + self + neighbors + sex, data=gerber, cp=0.0)
prp(CARTmodel3)

CARTmodel.control = rpart(voting ~ control,data=gerber,cp=0.0)
prp(CARTmodel.control,digits=6)
CARTmodel.controlsex = rpart(voting ~ control + sex, data = gerber,cp=0.0)
prp(CARTmodel.controlsex,digits=6)

Logmodel.controlsex = glm(voting ~ control+sex,data=gerber,family=binomial)
summary(Logmodel.controlsex)

Possibilities = data.frame(sex=c(0,0,1,1),control=c(0,1,0,1))
predict(Logmodel.controlsex, newdata=Possibilities, type="response")

Logmodel.new = glm(voting ~ sex + control + sex:control, data=gerber, family="binomial")

predict(Logmodel.new, newdata=Possibilities, type="response")
