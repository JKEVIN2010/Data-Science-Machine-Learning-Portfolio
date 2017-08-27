#In this program we practice the Machine learning algorithm K-nearest neightbor
#Load necessary packages

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(ggvis)


iris <- read.csv("../input/Iris.csv")

# Return all `iris` data
iris

# Return first 5 lines of `iris`
head(iris)

# Return structure of `iris`
str(iris)

# Division of `Species`
table(iris$Species) 

# Percentual division of `Species`
round(prop.table(table(iris$Species)) * 100, digits = 1)

# Summary overview of `iris`
summary(iris) 


# Build your own `normalize()` function
normalize <- function(x) {
  num <- x - min(x)
  denom <- max(x) - min(x)
  return (num/denom)
}

# Normalize the `iris` data
iris_norm <- as.data.frame(lapply(iris[1:4], normalize))

# Summarize `iris_norm`
summary(iris_norm)

#Shuffle the data

set.seed(1234)

# We choose a vector of 2 element and assign a value of 1 or 2 to the 150 rows of data set with proportions 0.67 and 0.33
ind <- sample(2, nrow(iris), replace=TRUE, prob=c(0.67, 0.33))

# Compose training set
iris.training <- iris[ind==1, 1:4]

# Inspect training set
head(iris.training)

# Compose test set
iris.test <- iris[ind==2, 1:4]

# Inspect test set
head(iris.test)

# Compose `iris` training labels
iris.trainLabels <- iris[ind==1,5]

# Inspect result
print(iris.trainLabels)

# Compose `iris` test labels
iris.testLabels <- iris[ind==2, 5]

# Inspect result
print(iris.testLabels)

#Load the library class which contains knn algorithm

library(class)

# Build the model using the K-nearest neighbor algorithm
iris_pred <- knn(train = iris.training, test = iris.test, cl = iris.trainLabels, k=3)

# Inspect `iris_pred`
iris_pred

# Put `iris.testLabels` in a data frame
irisTestLabels <- data.frame(iris.testLabels)

# Merge `iris_pred` and `iris.testLabels` 
merge <- data.frame(iris_pred,iris.testLabels)

# Specify column names for `merge`
names(merge) <- c("Predicted Species", "Observed Species")

# Inspect `merge` 
merge

#load gmodels
library(gmodels)

#Visualize and evaluate our model
CrossTable(x = iris.testLabels, y = iris_pred, prop.chisq=FALSE)