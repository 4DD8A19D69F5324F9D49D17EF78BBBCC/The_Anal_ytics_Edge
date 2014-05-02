library(maps)
library(ggplot2)
library(ggmap)
library(igraph)

edges = read.csv('edges.csv')
users = read.csv('users.csv')


df=data.frame(edges$V2,edges$V1)
colnames(df)= c('V1','V2')

edges2 = rbind(edges,df)
edges2 = unique(edges2)

edges2$x =1 

mean(aggregate(x ~ V2,data=edges2,FUN=sum)$x)

table(users$school,users$locale)

table(users$school,users$gender)

g = graph.data.frame(edges, FALSE, users)
plot(g, vertex.size=5, vertex.label=NA)
sum(degree(g)>=10)
V(g)$size = degree(g)/2+2
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$gender == "A"] = "red"
V(g)$color[V(g)$gender == "B"] = "gray"
plot(g, vertex.label=NA)

V(g)$color = "black"
V(g)$color[V(g)$school == "A"] = "red"
V(g)$color[V(g)$school == "AB"] = "gray"


V(g)$color = "black"
V(g)$color[V(g)$locale == "A"] = "red"
V(g)$color[V(g)$locale == "B"] = "gray"


?igraph.plotting
