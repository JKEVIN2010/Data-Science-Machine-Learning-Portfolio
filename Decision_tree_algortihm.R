# This program predict the survival in the Titanic Disaster using Decision Trees
# Decision Trees are powerful tools in Machine Learning
# Author: "Kevin Mekulu

library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(rpart) # Loading Decision Tree Package

# Import the training set: train

train <- read.csv("../input/train.csv")

# Import the testing set: test

test <- read.csv("../input/test.csv")

# Your train and test set are still loaded in
str(train)
str(test)

# Build the decision tree
my_tree_two <- rpart(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked, data = train, method = "class")

# Visualize the decision tree using plot() and text()
plot(my_tree_two)
text(my_tree_two)

# Load in the packages to build a fancy plot
library(rattle)
library(rpart.plot)
library(RColorBrewer)


# Time to plot your fancy tree
fancyRpartPlot(my_tree_two)

# my_tree_two and test are available in the workspace

# Make predictions on the test set
my_prediction <- predict(my_tree_two, newdata = test, type = "class")

# Finish the data.frame() call
my_solution <- data.frame(PassengerId = test$PassengerId, Survived = my_prediction)

# Use nrow() on my_solution
nrow(my_solution)


# Finish the write.csv() call
write.csv(my_solution, file = "my_solution.csv", row.names = FALSE)