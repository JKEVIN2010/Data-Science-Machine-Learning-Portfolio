library(openintro) #Contains the dataset email
library(dplyr) #For data manipulation
library(ggplot2) #For fancy plots

plot(email$num_char) #Plot to check outliers

#Compute summary statistics
email %>% 
  group_by(spam) %>%
  summarize(median(num_char), IQR(num_char))

#Create plot
email %>% 
  mutate(log_num_char = log(num_char)) %>%
  ggplot(aes(x = spam, y = log_num_char)) + geom_boxplot() + facet_wrap(~spam)


boxplot(email$exclaim_mess) #Quick outlier chec for the variable
# Compute center and spread for exclaim_mess by spam
email %>%
  group_by(spam) %>%
  summarize(median(exclaim_mess),IQR(exclaim_mess))#Use median and IQR due to outliers

# Create plot for spam and exclaim_mess
email %>%
  mutate(log_exclaim_mess = log(exclaim_mess + 0.01)) %>% 
  
  ggplot(aes(x = log_exclaim_mess)) + geom_histogram() + facet_wrap(~spam)

table(email$image)#Count of images per email

# Create plot of proportion of spam by image
email %>%
  mutate(has_image = image > 0) %>%
  ggplot(aes(x = has_image, fill = spam)) +
  geom_bar(position = "fill")

sum(email$image > email$attach) #Check if number of attach files greater than numer of images

# Analysis of the asociation between the dollar sign and spam
email %>%
  filter(dollar > 0) %>%
  group_by(spam) %>%
  summarize(median(dollar))

email %>%
  filter(dollar > 10) %>%
  group_by(spam) %>%
  ggplot(aes(x = spam)) + geom_bar()

#Reorder levels
email$number <- factor(email$number, levels = c("none","small","big"))

# Construct plot of number
email %>%
  group_by(spam) %>%
  ggplot(aes( x = number)) +  
  geom_bar() + 
  facet_wrap(~spam)



