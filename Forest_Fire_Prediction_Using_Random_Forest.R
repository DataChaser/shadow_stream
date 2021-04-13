#Author: James Johnson
#Title: Forest_Fire_Prediction_Using_Random_Forest

#Predicting the Algerian forest fires (Bejaia region) using the Random Forest Classification

#Setting the working directory
setwd('C:/Users/DELL/Desktop/Project_Files/Algerian_Forest_Fires')

#Importing the data and viewing the first few rows
dataset = readxl::read_excel('Algerian_Forest_Fires.xlsx')
head(dataset)

#Converting the dataset into a data frame
dataset = as.data.frame(dataset)

#Removing the unwanted 'Days' column from the dataset
dataset = dataset[-1]
View(dataset)
#Converting 'Classes' into a factor and encoding them
dataset$Classes = factor(dataset$Classes, levels = c('not fire', 'fire'), labels = c(0,1))
dataset
dim(dataset)

#Splitting the data into training and test sets
set.seed(123)  #The results would then be reproducible
library(caret)
train = createDataPartition(y = dataset$Classes, p = 0.75, list = FALSE) #75% into training set
#There is uneven distribution between 'fire' and 'not fire' which might bias the results
#createDataPartition will make sure both training and test sets has balanced observations of classes
training_set = dataset[train,]
test_set = dataset[-train,]
dim(training_set); dim(test_set) #dimensions of training_set and test_set
head(training_set); head(test_set)

#Conventional method of data splitting (which we won't be using)
#set.seed(1234)
#library(caTools)
#split = sample.split(dataset$Classes, SplitRatio = 0.75)
#training_set = subset(dataset, split == TRUE)
#test_set = subset(dataset, split == FALSE)
#dim(training_set); dim(test_set)
#head(training_set); head(test_set)  

#Using the Random Forest classification algorithm from the 'randomForest' package
suppressPackageStartupMessages(library(randomForest))  #supress used to supress messages in output
classifier = randomForest(data = training_set, x = training_set[-11], y = training_set$Classes, ntree = 10)

#Predicting the classifier for the 'test_set'
y_pred = predict(classifier, newdata = test_set, type = 'response')

#Checking model performance on the 'test_set' and checking the accuracy 
cm = table(test_set[,11], y_pred)
cm
accuracy = (cm[1,1] + cm[2,2])/(cm[1,1] + cm[2,2] + cm[1,2] + cm[2,1])
round(accuracy*100, 2) #in percentage #gives a 93.1% accuracy


#Another method for checking model performance by using the K-Fold Cross Validation Method 
#First loading the 'caret' package (already loaded) and performing the technique
set.seed(1234)   #results would be reproducible
suppressPackageStartupMessages(library(caret))  #to supress any messages
folds = createFolds(training_set$Classes, k = 10)
cv = lapply(folds, function(x){
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = randomForest(data = training_fold, x = training_fold[-11], y = training_fold$Classes, ntree = 10)
  y_pred = predict(classifier, newdata = test_fold, type = 'response')
  cm = table(test_fold[,11], y_pred)
  accuracy = (cm[1,1]+cm[2,2])/(cm[1,1]+cm[2,2]+cm[1,2]+cm[2,1])
  return(accuracy)
})

#Checking the 20 accuracies
cv

#Finding the mean (and final) model accuracy
round(mean(as.numeric(cv))*100, 2)  #gives 92.19% accuracy



#In the above steps, we took all the variables into the model (after excluding the 'Days' column)
#Now, we take only the significant variables by comparing each of them with the 'Classes'
#We do this using the Chi-Square Test and iterate over the variables using a For loop

suppressWarnings(for(i in 1:10){
  print(names(dataset[i]))
  print(chisq.test(dataset[i], dataset$Classes))
}) #SupressWarnings is used to supress the warning messages which might pop up in this code

#'Temperature', 'Rain', 'ISI', 'FWI' are the only variables which are significant

#We subset the dataset into these columns and look at them
data = dataset[c(1,4,8,10,11)]
data

#Splitting the Data 
set.seed(123)
#library(caret)
split = createDataPartition(y = data$Classes, p = 0.75, list = FALSE)
train_set = data[split,]
test_set = data[-split,]

#We go through the K-Fold Cross Validation Method to check the new model accuracy
set.seed(123)   #results would be reproducible
fold = createFolds(train_set$Classes, k = 20)
ab = lapply(fold, function(x){
  training_fold1 = train_set[-x, ]
  test_fold1 = train_set[x, ]
  classifier1 = randomForest(x = training_fold1[-5], y = training_fold1$Classes, data = training_fold1, ntree = 10)
  y_pred1 = predict(classifier1, newdata = test_fold1, type = 'response')
  cm1 = table(test_fold1[,5], y_pred1)
  accuracy1 = (cm1[1,1] + cm1[2,2]) / (cm1[1,1] + cm1[2,2] + cm1[1,2] + cm1[2,1])
  return(accuracy1)
})

ab

#Calculating the final accuracy
round(mean(as.numeric(ab))*100,2)

#The accuracy increases but only slightly (to 95.67%), but this is a good model anyway with an accuracy of more than 90% consistently