
setwd("~/")
data <- read.csv("StocksCluster.csv", header = TRUE)

#visualize the data
head(data)

#Check summary of the data
summary(data)

#Check for missing values
sum(is.na(data))

#Check dimension of the data
dim(data)

#Check the structure of the data
str(data)


#Visualize possible correlation within the data
library(corrgram)

#Variables are ordered using principal component analysis of the correlation matrix.
corrgram(data[,-12], order = T)

#Let's take a closer look at some highly correlated returns in the data
library(ggplot2)
ggplot(data[,-12], aes(x = data$ReturnJune, y = data$ReturnFeb)) +
  geom_point(size = 2.5, color = "navy") +
  ggtitle("Returns in June vs Returns in February")

ggplot(data[,-12], aes(x = data$ReturnOct, y = data$ReturnNov)) +
  geom_point(size = 2.5, color = "red") +
  ggtitle("Returns in October vs Returns in November")

ggplot(data[,-12], aes(x = data$ReturnOct, y = data$ReturnJan)) +
  geom_point(size = 2.5, color = "green") +
  ggtitle("Returns in June vs Returns in February")


#Visualize overall distribution of the data with histograms
colors <- c("red", "yellow", "green", "violet", "orange", 
            "blue", "pink", "cyan") 

par(mfrow = c(1,4))
for(i in 1:11)
{
  hist(data[,i], col = colors)
}

#Check outliers using boxplots
par(mfrow = c(1,4))
for(i in 1:11)
{
  boxplot(data[,i], col = colors)
}

#Apply Principal Component analysis to check variables with highest variance
library(mlbench)

library(caret)

#Pre-process parameters for the dataset
prep.prm <-  preProcess(data[,-12], method = c("center","scale","pca"))

print(prep.prm)#As expected all 11 variables of study are needed

#Splitting the data into training set(80%) and test set(20%)
index <- sample(1:nrow(data[,-12]), size = 0.8*nrow(data[,-12]))

train.data <- data[index,]

test.data <- data[-index,]

#Run the algorithms using 10-fold cross validation
control <- trainControl(method="cv", number=10)
metric <- "Accuracy"

#Fire up logistic regression
set.seed(1234)

fit.glm <- glm(PositiveDec~., data = train.data, family = "binomial" )

y.hat <- ifelse(predict(fit.glm,test.data,type="response") > 0.5,1,0)

misrate <- sum(y.hat != test.data[,12])/length(test.data[,12])

#The misclassification rate is very high let's test other algorithms
print(misrate)

# Fire up other algorithms
# LDA
set.seed(1234)
fit.lda <- train(factor(PositiveDec)~., data=train.data, method="lda", metric=metric, trControl=control)

#Naive Bayes
set.seed(1234)
fit.bayes <- train(factor(PositiveDec)~., data=train.data, method="naive_bayes", metric=metric, trControl=control)

#Logistic Regression with penalty(Lasso)
set.seed(1234)
fit.logit <- train(factor(PositiveDec)~., data=train.data, method="glmnet", metric=metric, trControl=control)

#Boosted Logistic regression
set.seed(1234)
fit.boot <- train(factor(PositiveDec)~., data=train.data, method="LogitBoost", metric=metric, trControl=control)



#Pick the best model
results <- resamples(list(lda = fit.lda, bayes = fit.bayes,  logistic = fit.logit))

#Lda is the most accurate with an accuracy of 57.17%
print(fit.lda)

#Make Predictions
predictions <- predict(fit.lda, test.data)

#Confusion Matrix
confusionMatrix(predictions, test.data[,12])

#Improve model with more powerful algorithms
#Fit stochastic gradient boosting
set.seed(1234)
fit.gbm <- train(factor(PositiveDec)~., data=train.data, method="gbm", metric=metric, trControl=control)
print(fit.gbm) #Accuracy improved to 57.9%!!!
