install.packages('caret')
library(caret)
airlines = read.csv('AirlinesCluster.csv')
colMeans(airlines)

preproc = preProcess(airlines)
airlinesNorm = predict(preproc, airlines)
sapply(airlinesNorm,min)
sapply(airlinesNorm,max)

airline.dist = dist(airlinesNorm,method='euclidian')
airline.hclust = hclust(airline.dist,method='ward.D')
plot(airline.hclust)
airline.cut = cutree(airline.hclust,k=5)
table(airline.cut)

for(i in 1:7){
  print(colnames(airlines)[i])
  print(tapply(airlines[,i],airline.cut,mean))
}

airline.kmeans = kmeans(airlinesNorm,centers=5,iter.max=1000)
table(airline.kmeans$cluster)
