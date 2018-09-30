View(quakes)
#applying the scale function to normalize the data in to smaller values (height of data frame)
zscore= as.data.frame( scale(trans["depth"] ))
print(minmax)
plot(quakes$depth)
plot(minmax)
plot(zscore)
print(normalize.decscale(trans))
plot(normalize.decscale(trans))
min_max<- function(x) 
{
  #it transforms the data in to minimum value
  return ((x - min(x)) / (max(x) - min(x)))
}
normalize.decscale<- function (data)
{
  #decimal scaling to a matrix or dataframe. Decimal scaling transforms the
  #data into [-1,1] by finding k such that the absolute value of the maximum
  #value of each attribute divided by 10^k is less than or equal to 1.
  maxvect <- apply(abs(data), 2, max)
  kvector <- ceiling(log10(maxvect))
  scalefactor <- 10^kvector
  scale(data, center = FALSE, scale = scalefactor)
}
#loading the builtin dataset
trans=head(quakes)
#applying thelapply function to normalize the data in to smaller values (height of data frame)
minmax = as.data.frame(lapply(trans["depth"], min_max))


#Discretization by Binning
bins<-5000
minimumVal<-min(quakes$depth)
maximumVal<-max(quakes$depth)
width=(maximumVal-minimumVal)/bins;
x<-cut(quakes$depth,breaks=seq(minimumVal, maximumVal, width))
x
barplot(quakes$depth)

barplot(table(cut(quakes$depth, breaks=seq(minimumVal, maximumVal, width))))

#Discretization by Histogram Analysis


hist(quakes$depth, prob=TRUE, col = "grey")
lines(density(quakes$depth), col = "blue")

#CLUSTERING
plot(depth~stations,quakes)

#Normalization
z<-quakes[,-c(1,1)]
m<-apply(z,2,mean)
s<-apply(z,2,sd)
z<-scale(z,m,s)

#Calculate Euclidean distance
distance<-dist(z)
print(distance,digits = 3)

#Cluster Dendrogram with complete linkage
hc.c<-hclust(distance)
plot(hc.c,hang=-1)

#Cluster Dendrogram with Average linkage
hc.a<-hclust(distance,method="average")
plot(hc.a,hang=-1)

#DECISION TREE

data<-quakes
str(data)
data$NSPF <- factor(data$stations)

#Partion data into Training and Validation Datasets
set.seed(1234)
pd<-sample(2,nrow(data),replace=TRUE,prob=c(0.8,0.2))
train<-data[pd==1,]
validate<-data[pd==2,]

#Decision Tree with Party
library(party)
tree<-ctree(NSPF~lat+long+mag,data=train,controls = ctree_control(mincriterion = 0.99,minsplit = 500))
tree
plot(tree)

#Predict
predict(tree,validate)

#Decision Tree with rPart
library(rpart)
tree1<-rpart(NSPF~lat+long+mag,train)
library(rpart.plot)
rpart.plot(tree1,extra=2)


#CORRELATION 
cor.test(quakes$depth,quakes$stations)

