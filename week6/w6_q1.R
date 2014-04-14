dailykos = read.csv('dailykos.csv')
dis = dist(dailykos[2:ncol(dailykos)],method='euclidean')
dailykos.clust = hclust(dis,method='ward.D')
dailykos.cut = cutree(dailykos.clust,k=7)

sum(dailykos.cut==3)
table(dailykos.cut)

clusters = list()

for(i in 1:7){
  clusters[[i]] = dailykos[dailykos.cut==i,]
}
tail(sort(colMeans(clusters[[1]][-1])))

for(i in 1:7){
  print(i)
  print(sort(colMeans(clusters[[i]][-1]),decreasing=TRUE)[1:5])
}

set.seed(1000)
dailykos.kmeans = kmeans(dailykos[-1],centers=7)

clusters.kmeans = list()

for(i in 1:7){
  clusters.kmeans[[i]]= dailykos[dailykos.kmeans$cluster==i,]
}

table(dailykos.kmeans$cluster)


