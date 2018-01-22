library(ggplot2)
#Load the data set
train <- read.csv("Train_UWu5bXk.csv")
test <- read.csv("Test_u94Q5KV.csv")

#Check dimensions
dim(train)
dim(test)

#Check variable types in train
str(train)
#Check for missing Values
table(is.na(train))

#Pinpoint columns with missing values
colSums(is.na(train))

#Summary of training data set
summary(train)

#Univariate Analysis
colors <- c("red", "yellow", "green", "violet", "orange", 
           "blue", "pink", "cyan") 
par(mfrow = c(2,1))
hist(train$Item_Visibility, col = colors)
hist(train$Item_Outlet_Sales, col = colors)

#Bivariate Analysis
ggplot(train, aes(x= Item_Visibility, y = Item_Outlet_Sales)) + 
  geom_point(size = 2.5, color="navy") + 
  xlab("Item Visibility") + ylab("Item Outlet Sales") + 
  ggtitle("Item Visibility vs Item Outlet Sales")

ggplot(train, aes(Outlet_Identifier, Item_Outlet_Sales)) + 
  geom_bar(stat = "identity", color = "cyan") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "black"))  + 
  ggtitle("Outlets vs Total Sales") + theme_bw()

ggplot(train, aes(Item_Type, Item_Outlet_Sales)) + 
  geom_bar( stat = "identity") +
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "navy")) + 
  xlab("Item Type") + ylab("Item Outlet Sales")+
  ggtitle("Item Type vs Sales")


ggplot(train, aes(Item_Type, Item_MRP)) +
  geom_boxplot() +
  ggtitle("Box Plot") + 
  theme(axis.text.x = element_text(angle = 70, vjust = 0.5, color = "red")) + 
  xlab("Item Type") + ylab("Item MRP") + 
  ggtitle("Item Type vs Item MRP")

#Merge Both Data Sets
test$Item_Outlet_Sales <- 1
join <- rbind(train,test)

#Use median to impute missing value because 
#it's very robust to outliers
join$Item_Weight[is.na(join$Item_Weight)]<- median(join$Item_Weight, na.rm = T)
table(is.na(join$Item_Weight))

#Consider item visibility. One of the item has visibility 0
#which is not feasible so we consider it as missing value
join$Item_Visibility <- ifelse(join$Item_Visibility == 0, 
                               median(join$Item_Visibility),join$Item_Visibility)

#Analysis of Categorical Variables
#Categorize the empty level in outlet size as other
levels(join$Outlet_Size)[1] <- "Other"

#Rename various levels of fat content
library(plyr)

join$Item_Fat_Content <- revalue(join$Item_Fat_Content,
                                 c("LF" = "Low Fat","reg" = "Regular"))

join$Item_Fat_Content <- revalue(join$Item_Fat_Content, c("low fat" = "Low Fat"))
table(join$Item_Fat_Content)

library(dplyr)
#Analyze Outlet Identifier
a <- join %>% group_by(Outlet_Identifier)%>%
  tally()

head(a)
names(a)[2] <- "Outlet_Count"
join <- full_join(a,join,by = "Outlet_Identifier")

#Count of item Identifier
b <- join %>% group_by(Item_Identifier) %>%
  tally()
names(b)[2] <- "Item_count"

head(b)

join <- merge(b,join,by = "Item_Identifier")

c <- join %>% 
  select(Outlet_Establishment_Year)%>%
  mutate(Outlet_Year = 2013 - join$Outlet_Establishment_Year)
head(c)
join <- full_join(c,join)

q <- substr(join$Item_Identifier,1,2)
q <- gsub("FD","Food",q)
q <- gsub("DR", "Drinks",q)
q <- gsub("NC", "Non-Consummable",q)
table(q)

join$Item_Type_New <- q

#Label encoding 0 for low fat, 1 for regular
join$Item_Fat_Content <- ifelse(join$Item_Fat_Content == "Regular",1,0)

sample <- select(combi, Outlet_Location_Type)
demo_sample <- data.frame(model.matrix(~.-1,sample))
head(demo_sample)

#Replace Categorical Variables with dummy variables(one hot encoding)
library(dummies)
join <- dummy.data.frame(join, names = c('Outlet_Size','Outlet_Location_Type',
                                           'Outlet_Type', 'Item_Type_New'),  sep='_')
str(join)

#Predictive MOdelling/Machine Learning Algorithms
#Delete columns converted into other variables and ID variables

join <- select(join, -c(Item_Identifier, Outlet_Identifier, Item_Fat_Content,Outlet_Establishment_Year,Item_Type))

#New train set and Test set
train.1 <- join[1:nrow(train),]
test.1 <- join[-(1:nrow(train)),]

#Deploy Linear Regression
linear_model <- lm(Item_Outlet_Sales ~ ., data = train.1)
#R^2 = 0.2093 which is poor the model can be improved
summary(linear_model)

#Check correlation between predictor values
cor(train.1)

#Outlet_Count is highly correlated (negatively) with Outlet Type Grocery Store

#Simplify our regression MOdel
