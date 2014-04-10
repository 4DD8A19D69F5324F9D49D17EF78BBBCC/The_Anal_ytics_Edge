library(rpart.plot)
library(rpart)
library(caTools)
library(randomForest)
acc = function(pred,real){
  sum(pred==real) / length(real)
}

letters = read.csv('Downloads/letters_ABPR.csv')




letters$isB = as.factor(letters$letter == "B")
set.seed(1000)
sp = sample.split(letters$isB,SplitRatio=0.5)
Train = letters[sp,]
Test = letters[!sp,]
CARTb = rpart(isB ~ . - letter  , data=Train, method="class")
acc(predict(CARTb,newdata=Test,type='class'),Test$isB)
set.seed(1000)
rfmodel = randomForest(isB ~ . - letter, data=Train)
acc(predict(rfmodel,newdata=Test),Test$isB)



letters$letter = as.factor( letters$letter )
letters$isB = NULL
set.seed(2000)
sp = sample.split(letters$letter,SplitRatio=0.5)
Train = letters[sp,]
Test = letters[!sp,]

cartmulti = rpart(letter ~ .,data = Train,method='class')
rfmulti = randomForest(letter ~ .,data = Train,method='class')
acc(predict(cartmulti,newdata=Test,type='class'),Test$letter)
acc(predict(rfmulti,newdata=Test,type='class'),Test$letter)

