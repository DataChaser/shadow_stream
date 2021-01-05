#Task 1: Prediction Using Supervised ML (Predicting the percentage of students based on the number of study hours)

#Setting Working Directory
setwd('C:/Users/DELL/Desktop/Internships/Sparks_Foundation')

#Importing the dataset (and Pre-Processing)
dataset = read.csv('Scores&Hours.csv')
head(dataset)
names(dataset)[1] = 'Hours'

#Looking at the data
View(dataset)

#Visualizing the data
library(ggplot2)
ggplot() + 
  geom_point(aes(x = dataset$Hours, y = dataset$Scores), color = 'black') +
  ggtitle('Hours vs Scores') +
  xlab('Hours Spent') +
  ylab('Score (in Percentage)')

#Checking the correlation 
cor(dataset$Hours, dataset$Scores)

#Splitting the dataset into Training and Test Set
library(caTools)
split = sample.split(dataset$Scores, SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)
View(training_set)
View(test_set)

#Fitting the Regression and generating results
regressor = lm(formula = Scores~Hours, data = training_set)
summary(regressor)

#Visualizing the training_set with the regression line
ggplot() + 
  geom_point(aes(x = training_set$Hours, y = training_set$Scores), color = 'black') +
  geom_line(aes(x = training_set$Hours, y = predict(regressor, newdata = training_set)), color = 'red') +
  ggtitle('Hours vs Scores(with Regression Line') +
  xlab('Hours Spent') +
  ylab('Score (in Percentage)')

#Visualizing the test_set with the regression line
ggplot() + 
  geom_point(aes(x = test_set$Hours, y = test_set$Scores), color = 'black') +
  geom_line(aes(x = training_set$Hours, y = predict(regressor, newdata = training_set)), color = 'red') +
  ggtitle('Hours vs Scores(with Regression Line') +
  xlab('Hours Spent') +
  ylab('Score (in Percentage)')

#Predicting the Percentage of student who studied for 9.25 hours per day
predicted_score = predict(regressor, newdata = data.frame(Hours = 9.25))
predicted_score
