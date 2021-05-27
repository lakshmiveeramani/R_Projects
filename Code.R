#load necessary libraries and automatically install if any are missing
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(broom)) install.packages("broom", repos = "http://cran.us.r-project.org")
if(!require(rpart)) install.packages("rpart", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(ggpubr)) install.packages("ggpubr", repos = "http://cran.us.r-project.org")

#create file objects to store the downloaded csv files
dlRed <- tempfile()
dlWhite <- tempfile()

#download the data sets for red and white wines by accessing them online
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-red.csv", dlRed)
download.file("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", dlWhite)

#convert the data sets into data frames from the downloaded csv files, use semicolon as the field separator
dfRed <- read.csv(dlRed, header = TRUE, sep = ";")
dfWhite <- read.csv(dlWhite, header = TRUE, sep = ";")

#assign the color to the data frames to distinguish between the colors in the combined data frame
dfRed <- cbind(dfRed, color = "Red")
dfWhite <- cbind(dfWhite, color = "White")

#combine the two data frames into one master set
dfMaster <- rbind(dfRed, dfWhite)

#Set a random seed and break up the master data set into two sets: train and test with a 9:1 ratio of 
#the number or records inside these data sets.  The train set will be used to train the data model 
#to predict wine quality and the test set will be used to test the trained models
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
test_index <- createDataPartition(y = dfMaster$quality, times = 1, p = 0.1, list = FALSE)
dfTrain <- dfMaster[-test_index,]
dfTest <- dfMaster[test_index,]

#Data Exploration, look at the sample of the data set
head(dfTest)

#Univariate Analysis

#divide the plot screen into a table of 3 rows by 4 columns
par(mfrow=c(3,4))

#check the data distribution of the fixed acidity variable with a boxplot to see the spread and the outliers
boxplot(fixed.acidity~color, data=dfTrain, main="Fixed Acidity")

#check the data distribution of the volatile acidity variable with a boxplot to see the spread and the outliers
boxplot(volatile.acidity~color, data=dfTrain, main="Volatile Acidity")

#check the data distribution of the citric acid variable with a boxplot to see the spread and the outliers
boxplot(citric.acid~color, data=dfTrain, main="Citric Acid")

#check the data distribution of the residual sugar variable with a boxplot to see the spread and the outliers
boxplot(residual.sugar~color, data=dfTrain, main="Residual Sugar")

#check the data distribution of the chlorides variable with a boxplot to see the spread and the outliers
boxplot(chlorides~color, data=dfTrain, main="Chlorides")

#check the data distribution of the free sulfur dioxide variable with a boxplot to see the spread and the outliers
boxplot(free.sulfur.dioxide~color, data=dfTrain, main="Free Sulfur Dioxide")

#check the data distribution of the total sulfur dioxide variable with a boxplot to see the spread and the outliers
boxplot(total.sulfur.dioxide~color, data=dfTrain, main="Total Sulfur Dioxide")

#check the data distribution of the density variable with a boxplot to see the spread and the outliers
boxplot(density~color, data=dfTrain, main="Density")

#check the data distribution of the ph variable with a boxplot to see the spread and the outliers
boxplot(pH~color, data=dfTrain, main="PH")

#check the data distribution of the sulphates variable with a boxplot to see the spread and the outliers
boxplot(sulphates~color, data=dfTrain, main="Sulphates")

#check the data distribution of the alcohol variable with a boxplot to see the spread and the outliers
boxplot(alcohol~color, data=dfTrain, main="Alcohol")

#divide the plot screen into a table of 3 rows by 4 columns
par(mfrow=c(3,4))

#check the data distribution of the fixed acidity variable with a histogram to idenify distribution shape an skewedness
hfa <- dfTrain %>%
  ggplot(aes(x=fixed.acidity, fill=color)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  labs(fill="")

#check the data distribution of the fixed volatile variable with a histogram to idenify distribution shape an skewedness
hva <- dfTrain %>%
  ggplot(aes(x=volatile.acidity, fill=color)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  labs(fill="")

#check the data distribution of the citric acid variable with a histogram to idenify distribution shape an skewedness
hca <- dfTrain %>%
  ggplot(aes(x=citric.acid, fill=color)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  labs(fill="")

#check the data distribution of the residual sugar variable with a histogram to idenify distribution shape an skewedness
hrs <- dfTrain %>%
  ggplot(aes(x=residual.sugar, fill=color)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  labs(fill="")

#check the data distribution of the chlorides variable with a histogram to idenify distribution shape an skewedness
hc <- dfTrain %>%
  ggplot(aes(x=chlorides, fill=color)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  labs(fill="")

#check the data distribution of the sulfur dioxide variable with a histogram to idenify distribution shape an skewedness
hfsd <- dfTrain %>%
  ggplot(aes(x=free.sulfur.dioxide, fill=color)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  labs(fill="")

#check the data distribution of the sulfur dioxide variable with a histogram to idenify distribution shape an skewedness
htsd <- dfTrain %>%
  ggplot(aes(x=total.sulfur.dioxide, fill=color)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  labs(fill="")

#check the data distribution of the density variable with a histogram to idenify distribution shape an skewedness
hd <- dfTrain %>%
  ggplot(aes(x=density, fill=color)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  labs(fill="")

#check the data distribution of the pH variable with a histogram to idenify distribution shape an skewedness
hph <- dfTrain %>%
  ggplot(aes(x=pH, fill=color)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  labs(fill="")

#check the data distribution of the sulphates variable with a histogram to idenify distribution shape an skewedness
hs <- dfTrain %>%
  ggplot(aes(x=sulphates, fill=color)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  labs(fill="")

#check the data distribution of the alcohol variable with a histogram to idenify distribution shape an skewedness
ha <- dfTrain %>%
  ggplot(aes(x=alcohol, fill=color)) +
  geom_histogram(color="#e9ecef", alpha=0.6, position='identity') +
  labs(fill="")

#display the histograms in a 4x4 grid
ggarrange(hfa, hva, hca, hrs)
ggarrange(hc, hfsd, htsd, hd)
ggarrange(hph, hs, ha)

#Bi-variate analysis 
#identify any visible correlation between fixed acidity and quality
pfa <- ggplot(dfTrain, aes(x=fixed.acidity, y=quality)) + geom_point()

#identify any visible correlation between volatile acidity and quality
pva <- ggplot(dfTrain, aes(x=volatile.acidity, y=quality)) + geom_point()

#identify any visible correlation between citric acid and quality
pca <- ggplot(dfTrain, aes(x=citric.acid, y=quality)) + geom_point()

#identify any visible correlation between residual sugar and quality
prs <- ggplot(dfTrain, aes(x=residual.sugar, y=quality)) + geom_point()

#identify any visible correlation between chlorides and quality
pc <- ggplot(dfTrain, aes(x=chlorides, y=quality)) + geom_point()

#identify any visible correlation between free sulfur dioxide and quality
pfsd <- ggplot(dfTrain, aes(x=free.sulfur.dioxide, y=quality)) + geom_point()

#identify any visible correlation between total sulfur dioxide and quality
ptsd <- ggplot(dfTrain, aes(x=total.sulfur.dioxide, y=quality)) + geom_point()

#identify any visible correlation between density and quality
pd <- ggplot(dfTrain, aes(x=density, y=quality)) + geom_point()

#identify any visible correlation between pH and quality
pph <- ggplot(dfTrain, aes(x=pH, y=quality)) + geom_point()

#identify any visible correlation between sulphates and quality
ps <- ggplot(dfTrain, aes(x=sulphates, y=quality)) + geom_point()

#identify any visible correlation between alcohol and quality
pa <- ggplot(dfTrain, aes(x=alcohol, y=quality)) + geom_point()

#visualize the plots in a 3x4 grid
ggarrange(pfa, pva, pca, prs, pc, pfsd, ptsd, pd, pph, ps, pa)


#check if any values are missing in the data frame by checking if any of fields are empty
#and summing up the result, anything greater than zero would indicate missing values
sum(is.na(dfTrain$fixed.acidity))
sum(is.na(dfTrain$volatile.acidity))
sum(is.na(dfTrain$citric.acid))
sum(is.na(dfTrain$residual.sugar))
sum(is.na(dfTrain$chlorides))
sum(is.na(dfTrain$free.sulfur.dioxide))
sum(is.na(dfTrain$total.sulfur.dioxide))
sum(is.na(dfTrain$density))
sum(is.na(dfTrain$pH))
sum(is.na(dfTrain$sulphates))
sum(is.na(dfTrain$alcohol))
sum(is.na(dfTrain$quality))

#add a quality.factor column to the data frame, which is a converted data type and value of the
#quality column from numeric to factor, in order to be used in the confusion matrix processing below
dfTrain <- dfTrain %>% mutate(quality.factor=as.factor(quality))
dfTest <- dfTest %>% mutate(quality.factor=as.factor(quality))

#train a k nearest neighbors model by using all the variables and the train dataset
train_knn <- train(quality.factor~color+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, method="knn", data=dfTrain)

#visualize the trained model
plot(train_knn)

#create a prediction set by using the trained knn model against the test set
y_hat_knn <- predict(train_knn, dfTest, type="raw")

#check the accuracy of the model when applied against the test set quality
confusionMatrix(data=y_hat_knn, reference=dfTest$quality.factor)$overall[["Accuracy"]]

#train a linear discriminate analysis model by using all the variables and the train dataset
train_lda <- train(quality.factor~color+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, method="lda", data=dfTrain)

#create a prediction set by using the trained lda model against the test set
y_hat_lda <- predict(train_lda, dfTest)

#check the accuracy of the model when applied against the test set quality
confusionMatrix(data=y_hat_lda, reference=dfTest$quality.factor)$overall[["Accuracy"]]


#create a classification tree to observe the effect of the variables on quality 
#with setting a complexity parameter to 0.004 which does not make tree it overly complex and discernible on a plot
fit_tree <- rpart(quality.factor~color+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data = dfTrain, control=rpart.control(cp=0.004, minsplit = 1))

#reset the plot screen to display a single visual on the whole grid
par(mfrow=c(1,1))

#plot and label the resulting tree
plot(fit_tree)
text(fit_tree, cex=0.75)

#train a classification tree model by using all the variables and the train dataset
train_rpart <- train(quality.factor~color+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, method="rpart", data=dfTrain)

#visualize the trained rpart model
plot(train_rpart)

#create a prediction set by using the trained rpart model against the test set
y_hat_rpart <- predict(train_rpart, dfTest)

#check the accuracy of the model when applied against the test set quality
confusionMatrix(data=y_hat_rpart, reference=dfTest$quality.factor)$overall[["Accuracy"]]

#train a random forest model by using all the variables and the train dataset
train_rf <- randomForest(quality.factor~color+fixed.acidity+volatile.acidity+citric.acid+residual.sugar+chlorides+free.sulfur.dioxide+total.sulfur.dioxide+density+pH+sulphates+alcohol, data=dfTrain)

#create a prediction set by using the trained random forest model against the test set
y_hat_rf <- predict(train_rf, dfTest)

#check the accuracy of the model when applied against the test set quality
confusionMatrix(data=y_hat_rf, reference=dfTest$quality.factor)$overall[["Accuracy"]]
