library(maps)
library(ggplot2)
library(ggmap)

parole = read.csv('parole.csv')
parole$male = as.factor(parole$male)
parole$state = as.factor(parole$state)
parole$crime = as.factor(parole$crime)


table(parole$violator,parole$male)
14/(14+64)


ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5,color="blue")
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5,color="white") + facet_grid(male ~ .)
ggplot(data = parole, aes(x = age)) + geom_histogram(binwidth = 5,color="white") + facet_grid(. ~ male)
ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5)


ggplot(data = parole, aes(x = age, fill = male)) + geom_histogram(binwidth = 5,position="identity",alpha=0.5)


ggplot(data = parole, aes(x = time.served, fill = male)) + geom_histogram(binwidth = 0.1,position="identity",alpha=0.5)

ggplot(data = parole, aes(x = time.served, fill = crime)) + geom_histogram(binwidth = 1) +  facet_grid(. ~ crime)

ggplot(data = parole, aes(x = time.served, fill = crime)) + geom_histogram(binwidth = 1,position='identity',alpha=0.5)
