#Homework# 3
#Problem#1
library("MASS")
#Question a)
dat <- read.table("flights.txt")
dat.mds <- isoMDS(dist(dat))
x11()
layout(matrix(1:2, nrow=1, ncol=2))
plot(dat.mds$points, pch=as.character(c(1:nrow(dat))), main="MDS")

#Question b)
#First, let's take a look at a geographic map and compare
#that to our MDS scatterplot
#mY guesses would be Madison = 5, Minneapolis = 4, Las Vegas = 6
#We rotated the plot to get a better comparison with the actual map
#and Used distances between the cities on the real map to make my guess
#For example last vegas and New York are both on opposite sides on the real map
#So on the scatter plot they would correspond to 6 and 2 respectively
#MDS is different than the real map because two cities like
#South Bend and Madison might be close to each other
#But when taking a flight the plane might go to a bigger city like Minneapolis  first 
#Before going to Madison which takes longer

#Problem#2
#2.1
load("zip.RData")

library("RDRToolbox")

dat$train <- dat$train[which(dat$train[,1] == 7),-1]

dim(dat$train)

dat$train <- as.matrix(dat$train)

dat.isomap <- Isomap(data = dat$train, dims = 2, k = 5)
x11()


dim2 <- dat.isomap$dim2

get.grid.points <- function(dim2, m=5)
{
  seqx <- seq(from=min(dim2[, 1]), to=max(dim2[, 1]), length.out=m+2)[2 : (m+1)]
  seqy <- seq(from=min(dim2[, 2]), to=max(dim2[, 2]), length.out=m+2)[2 : (m+1)]
  seqgrid <- expand.grid(seqx, seqy)
  
  ind <- rep(NA, nrow(seqgrid))
  for (i in 1 : nrow(seqgrid))
  {
    ind[i] <- which.min((dim2[, 1] - seqgrid[i, 1]) ^ 2 + (dim2[, 2] - seqgrid[i, 2]) ^ 2)
  }
  
  return(ind=ind)
}

res.grid <- get.grid.points(dim2)

x11()
plot.scatter <- function(dim2, res.grid)
{
  plot(dim2)
  points(dim2[res.grid, ], col='red', pch=13)
}
plot.scatter(dim2, res.grid)

conv.image <- function(vec)
{
  mat <- matrix(as.numeric(vec), nrow = 16, ncol = 16)
  mat <- -mat[, 16:1]
  par(mar = c(0, 0, 0, 0))
  image(mat, col = gray(seq(0, 1, 0.01)), xaxt = 'n', yaxt = 'n')
  
}

plot.images <- function(images, res.grid)
{
  layout(matrix(1 : length(res.grid), nrow=sqrt(length(res.grid)), byrow=TRUE))
  for (i in 1 : length(res.grid))
  {
    conv.image(images[res.grid[i], ])		
  }
}
x11()
plot.images(dat$train, res.grid)

#2.3
#We observe from the plot that on the right side of the 
#diagonal(first three rows and last three columds) we can clearly see the digits
#While on the left side of the diagonal the digits are not as clear


#Problem #3
library("class")

set.seed(10)

#3.1
k_test <- knn(dat$train[,-1], dat$test[,-1], dat$train[,1], k=1, prob=FALSE)

#3.2
confmat.test <- table(k_test, dat$test[,1]) 
print(confmat.test)
#k_test   4   7   9
#     4 189   5   3
#     7   2 141   5
#     9   9   1 169

misrate.test <- sum(k_test != dat$test[,1]) / length(dat$test[,1]) 
print(misrate.test) #The error is 0.04770992


#3.3
temp <- rep(0,10)
for(i in 1:10){
  k_test <- knn(dat$train[,-1], dat$test[,-1], dat$train[,1], k=i, prob=FALSE)
  
  temp[i] <- sum(k_test != dat$test[,1])/length(dat$test[,1])
}
x11()
plot(c(1:10),temp, xlab = "Values of K", ylab = "Error Rate",
main = "Plot of error rate vs Values of K")
lines(x = c(1:10), y = temp, col = "green")


#Problem 4
##QDA Classifier
library("MASS")
#4.1
train.qda <- qda(V1~.,data = dat$train ) #We get an error, we stop

#4.2
train.lda <- lda(V1~., data = dat$train) #We get an error 

train.lda<- lda(V1~., data = dat$train[,-210]) #Delete column #210

y.hat <- predict(train.lda, dat$test)$class

confmat <- table(y.hat,dat$test$V1)
print(confmat)
#y.hat   4   7   9
#    4 189   9   5
#    7   1 133   4
#    9  10   5 168


errate <- sum(y.hat != dat$test$V1)/length(dat$test$V1)
print(errate)#The error rate is 0.0648855

#4.3
digits.7 <- which(y.hat == 4 & dat$test[,1] == 9)

digits.9 <- which(y.hat == 7 & dat$test[,1] == 9)

layout(matrix(1:length(digits.7), nrow = 1, byrow = TRUE))
for (i in 1:length(digits.7)){
  conv.image(dat$test[digits.7[i],-1])
}

layout(matrix(1:length(digits.9), nrow = 1, byrow = TRUE))

for (i in 1:length(digits.9)){
  conv.image(dat$test[digits.9[i],-1])
}

#4.4
library(Matrix)
qr(dat$train[,-1])$rank

# We try to find out the rank of the matrix after removing one predictor
rank.k <- rep(0,ncol(dat$train[,-1]))
for(i in 1:ncol(dat$train[,-1])){
  rank.k[i] <- qr(dat$train[,-1][,-i])$rank
}
which(rank.k == max(rank.k))

#We run the LDA again on the train set deleting columns 210, 211,277,243
train.lda2 <- lda(V1~.-V210, data = dat$train[,-c(211,227,243)])

y.hat <- predict(train.lda2, dat$test)$class
confmat <- table(y.hat, dat$test$V1)
print(confmat)
#y.hat   4   7   9
#    4 189   9   5
#    7   1 133   4
#    9  10   5 168

errate <- sum(y.hat != dat$test$V1)/length(dat$test$V1)
print(errate)



#Problem#6
library("e1071")

#4.1
#Number of Classifiers: (k-1) + k*2*p = 1538 where k = 3 and p = 256
#Yes, the data is independently Gaussian distributed within every class

#4.2
train.naiv <- naiveBayes(V1~., data = dat$train)

label <- predict(train.naiv, newdata = dat$test)

table(label,dat$test$V1) #Confusion Matrix

sum(label != dat$test$V1)/length(label) #The error is 0.2881679 

#4.3
#This  naive Bayes classifier performs worse than the LDA because 
#the naive Bayes classifier assumes that the distribution of the data
#is independent but in this case the training data represents pixels of an image
#so many columns are correlated/dependent which violates a key assumption of
#the Bayes Classifier about independence of features so the Bayes classifier 
#performs poorly compare to LDA which assumes correlation between features

#4.4
# we will use the Boruta  Package in R to accomplish this variable selection
#Methodology:
#Firstly, it adds randomness to the given data set by creating shuffled 
#copies of all features (which are called shadow features).
#Then, it trains a random forest classifier on the extended data set and 
#applies a feature importance measure (the default is Mean Decrease Accuracy) 
#to evaluate the importance of each feature where higher means more important.
#At every iteration, it checks whether a real feature has a higher importance than the best of 
#its shadow features (i.e. whether the feature has 
#a higher Z score than the maximum Z score of its shadow features) and 
#constantly removes features which are deemed highly unimportant.
#Finally, the algorithm stops either when all features gets confirmed or rejected or 
#it reaches a specified limit of random forest runs.

library("Boruta") #Load the package

#Run Boruta algorithm on the training set using Boruta function
boruta.train <- Boruta(V1~., data = dat$train, doTrace = 2)

#The rejected columns from the boruta algorithm are: V230,V98,V128,V146,V16,V50
#V145,V176,V179,V51

#Now wer run Naives Bayes deleteting those columns
train.naiv <- naiveBayes(V1~., data = dat$train[,-c(V230,V98,V128,V146,
                                                    V16,V50,V145,V176,V179,V51)])

label <- predict(train.naiv, newdata = dat$test)

sum(label != dat$test$V1)/length(label) #The new error is 0.28622595!!!!!!!

#It's clear that the new error(0.28622595) is smaller than the old error(0.2881679 )


