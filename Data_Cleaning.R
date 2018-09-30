# Set the Working Directory
setwd("D:/Studies/VIII Semester/R/Programs/Ex 3")

#Get the Working Directory
getwd()  

#Read  the Dataset of CSV  Format
data_cleaning <- read.csv("D:/Studies/VIII Semester/R/Programs/Ex 3/rdy.csv", header=TRUE)

View(data_cleaning)
class(data_cleaning)
dim(data_cleaning)
length(data_cleaning) 
names(data_cleaning) 
nrow(data_cleaning) 
str(data_cleaning) 
min(data_cleaning$Budget_2018_2019_Total)
max(data_cleaning$Budget_2018_2019_Total)
mean(data_cleaning$Budget_2018_2019_Total)
sd(data_cleaning$Budget_2018_2019_Total)
summary(data_cleaning)
head(data_cleaning, n=10) 
tail(data_cleaning,n=10)
fix(data_cleaning)
library(dplyr)
glimpse(data_cleaning$Budget_2018_2019_Total)
table(data_cleaning$Budget_2018_2019_Total, data_cleaning$Budget_2018_2019_Total)
#Outlier Analysis
data_cleaning$Actual_2016_2017_Capital[data_cleaning$Actual_2016_2017_Capital>200]<-100
data_cleaning$Actual_2016_2017_Capital[data_cleaning$Actual_2016_2017_Capital<0]<-100

#Missing Values
#To replace the data with mean value
for (i in which(sapply(data_cleaning, is.numeric))) 
{
  data_cleaning[is.na(data_cleaning[, i]), i] <- mean(data_cleaning[, i], na.rm = TRUE)
}
#Ignoring the tuples
data_new<-na.omit(data_cleaning)
data_new
fix(data_new)
#binning
bins<-5
minimumVal<- -2100.00
maximumVal<- 982.72   
width=(maximumVal-minimumVal)/bins;
x<-cut(data_cleaning$Budget_2017_2018_Total, breaks=seq(minimumVal, maximumVal, width))
x
barplot(data_cleaning$Budget_2018_2019_Total)

barplot(table(cut(data_cleaning$Budget_2018_2019_Total, breaks=seq(minimumVal, maximumVal, width))))

#regression
scatter.smooth(x=data_cleaning$Revised_2017_2018_Revenue, y=data_cleaning$Budget_2018_2019_Revenue, main="Revised_2018_2019_Revenue ~ Budget_2018_2019_Revenue")
boxplot(data_cleaning$Budget_2018_2019_Total, main="Budget_2017_2018_Revenue", sub=paste("Outlier rows: ", boxplot.stats(data_cleaning$Budget_2017_2018_Revenue)$out))  # box plot for 'Budget_2017_2018_Revenue'
#pictorial representation
hist(data_new$Budget_2018_2019_Total)
plot(data_cleaning$Budget_2017_2018_Total,data_cleaning$Budget_2018_2019_Total)
boxplot(data_new)
boxplot(data_cleaning, las = 2,col = c("red","sienna","red","sienna","royalblue2","red","sienna","royalblue2","red","sienna","royalblue2","red","sienna","royalblue2"),at = c(1,2, 4,5,6, 8,9,10, 12,13,14, 16,17,18),par(mar = c(14, 5, 4, 2)- 1),names = c("Index","Partriculars","Actual_2016_2017_Revenue","Actual_2016_2017_Capital","Actual_2016_2017_Total","Budget_2017_2018_Revenue","Budget_2017_2018_Capital","Budget_2017_2018_Total","Revised_2017_2018_Revenue","Revised_2017_2018_Capital","Revised_2017_2018_Total","Budget_2018_2019_Revenue","Budget_2018_2019_Capital","Budget_2018_2019_Total"))
