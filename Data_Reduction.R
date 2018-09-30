# Set the Working Directory
setwd("D:/Studies/VIII Semester/R/Programs/Ex 4")

#Get the Working Directory
getwd()  
#Read  the Dataset of CSV  Format
data_reduction <- read.csv("D:/Studies/VIII Semester/R/Programs/Ex 4/congenital_data_2011.csv", header=TRUE)
View(data_reduction)
#DIMENSIONALITY REDUCTION
#Principal Components Analysis
# load data
data_reduction=read.csv("congenital_data_2011.csv")

# applying principal component analysis on  dataset
pca = prcomp(data_reduction) 

# plot to show variable importance 
par(mar = rep(2, 4)) 

# To know the values of pca
pca

# To plot the values
plot(pca) 

# plot pca components using biplot in r
biplot (pca , scale =0) 

# Rotating pca components
pca$rotation=-pca$rotation
pca$x=-pca$x

# plot pca components using biplot in r after rotating
biplot (pca , scale =0)

#NUMEROSITY REDUCTION
#Parametric Methods
#Regression(Linear Regression)
# To assign the field to a varible
x<-data_reduction$Paediatric
y<-data_reduction$AlivePaed30d


# create the relationship model using lm()
relation<-lm(y~x)
relation

# get the summary of the relationship
print (summary(relation))

# To plot a value by using x,y
plot(x,y)

# predict() to find the value of tickets booked
# x is the predictor variable and y is the response variable
a<-data.frame(x=100)
result<-predict(relation,a)
print(result)

# visualizing the regression graphically
abline (relation)

#Regression(Multiple Regression)

#selecting three columns
x=data_reduction[,1:4]
#assinging attributes data to frame
y=x$Procedures
x1=x$Paediatric
x2=x$AlivePaed30d
x3= x$DeadPaed30d
#using formula of regression
fit <- lm( y~  x1+  x2+  x3)
print(summary(fit)) # show results
plot(fit)
#prediction of future
newdata=data.frame(x1=150,x2=80,x3=70)
m=predict(fit,newdata)
print(paste("Paediatric",newdata$x1,"AlivePaed30d",newdata$x2,"DeadPaed30d",newdata$x3,"in the Procedures be",m ))

#Non-Parametric Methods

#Histogram
x<-data_reduction$Procedure
#Distributing Buckets equally
x2<-split(x,cut(x,5))
x2

x1<-tapply(x,cut(x,5),median)
x1

hist(data_reduction$Procedure,main="Before reduction",xlab="mean width",ylab="mean frequency",label="bucket",col="blue")
hist(x1,main="Before reduction",xlab="mean width",ylab="mean frequency",label="bucket",col="blue")

#Clustering
plot(data_reduction$Procedure,data_reduction$Paediatric)

#Normalization
z<-data_reduction[,-c(1,1)]
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
