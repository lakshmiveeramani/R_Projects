

library(tidyverse)

# Loading the data
temp <- tempfile()  
#url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank-additional.zip"
url <- "https://archive.ics.uci.edu/ml/machine-learning-databases/00222/bank.zip"

download.file(url = url,
              destfile = temp)

#data <- readr::read_delim(unz(temp2, "bank-additional-full.csv"), delim =";")
data <- readr::read_delim(unz(temp, "bank-full.csv"), delim =";")


# Making the data tidy and ready.

data$job = as.factor(data$job) # Type of job (Categorical)
data$marital = as.factor(data$marital)  # Maritial state (Categorical)
data$education = as.factor(data$education)  # Education level 
data$default = as.factor(data$default)   # Has credit in default? (Yes/No/Unknown)
data$housing = as.factor(data$housing)  # Has house loan? (Yes/No)
data$loan = as.factor(data$loan) # Has personal loan? (Yes/No/Unknown)
data$contact = as.factor(data$contact) # Type of contact (celluar, telephone)
data$month = as.factor(data$month) # Last month of contact
data$poutcome = as.factor(data$poutcome)  # outcome of the previous marketing campaign
data$y = as.factor(data$y)  # has the client subscribed a term deposit? (Target)

# Explaratory data analysis

# Distribution of age
data %>% 
  ggplot(aes(x = age, fill = y)) +
  geom_density(alpha = 0.2) +
  labs(title = "Distribution of the customers by age",
       subtitle= "Per Predictor state",
       x = "Age of the customer",
       y = "Density of customers",
       fill = "Predictor status")

# Distributiuon of jobs
data %>%
  group_by(job, y) %>%
  summarise(count = n()) %>%
  ggplot(aes(x = reorder(job, count), y = count, fill = y)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(title = "Distribution of the customers by job",
       subtitle= "Per Predictor state",
       x = "Jobs",
       y = "count",
       fill = "Predictor status")

# Chance of accepting the offer per job

data %>% filter(y == "yes") %>%
  group_by(job) %>%
  summarise(yes = n()) %>% 
  left_join(y = 
              data %>% group_by(job) %>%
              summarise(total = n()),
            by = "job") %>%
  mutate(Share = yes * 100 / total) %>%
  select(job, Share) %>%
  arrange(desc(Share)) %>%
  ggplot(aes(x = reorder(job, Share), y = Share)) +
  geom_bar(stat = "identity",color = "darkgreen",
           fill = "lightblue") +
  coord_flip() +
  labs(x = "Job",
       y = "Share of saying 'Yes' To the offer",
       title = "Chance of accepting the offer per job")

# Maritial Status
data %>%
  ggplot(aes(x = marital, fill = y))+
  geom_bar(stat = "count",
           position = "stack")+
  labs(title = "The distribution of maritial states",
       y = "Count")


# Chance of accepting the offer by maritial status

data %>%
  ggplot(aes(x = marital, fill = y))+
  geom_bar(stat = "count",
           position = "fill")+
  labs(title = "The chance of accepting the offer in different maritial states",
       y = "Rate")

# Education of the customers
data %>%
  ggplot(aes(x = education, fill = y)) +
  geom_bar()+
  labs(title = "Distribution of education between customers",
       y = "Count")

# Chance of accepting the offer by education 
data %>%
  ggplot(aes(x = education, fill = y)) +
  geom_bar(position = "fill")+
  labs(title = "Chance of accepting the offer between customers",
       y = "Count")


# Distribution of having load
data %>%
  ggplot(aes(x = default, fill = y ))+
  geom_bar(position = "dodge")+
  labs(x = "Has the credit by default?",
       title = "Distribution of having loan by the credit state")+
  geom_text(aes(label = ..count..),
            stat = "count",
            position = position_dodge(width = 0.7))

# Chance of accepting the offer by loan status

data %>%
  ggplot(aes(x = default, fill = y ))+
  geom_bar(position = "fill")+
  labs(x = "Has the credit by default?",
       title = "Chance of accepting the offer by having the credit")


# Customer's balance
data %>%
  ggplot(aes(x = balance))+
  geom_histogram(binwidth = 2000)+
  labs(title = "Historgram of customer's balance")


# Customer's balance overview
data$balance %>% summary()

# Housing and personal loan
data %>%
  ggplot(aes(x = housing, fill = y))+
  geom_bar()+
  facet_wrap(~ loan)+
  labs(title = "Distribution of having personal or home loan",
       subtitle = "facet by personal loan")

# Chancel of accepting the offer by personal loan
data %>%
  ggplot(aes(x = housing, fill = y))+
  geom_bar(position = "fill")+
  facet_wrap(~ loan)+
  labs(title = "Chance of accepting the offer by having home loan",
       subtitle = "facet by having personal loan",
       x = "Have the customer a housing loan?",
       y = "Chance")

# Contact type
data %>%
  ggplot(aes(x = contact, fill = y))+
  geom_bar()+
  labs(title = "Distribution of contact type")

# Chance of accepting the offer by contact type
data %>%
  ggplot(aes(x = contact, fill = y))+
  geom_bar(position = "fill")+
  labs(title = "The chance of accepting the offer by contact type")

# Duration of the call
data %>%
  ggplot(aes(x = duration, fill = y))+
  geom_density(alpha = 0.3)+
  labs(title = "Duration of the call distribution",
       subtitle = "By campaign status")

# A glance at duration
data$duration %>% summary()

# Distribution of the contacts
data %>%
  ggplot(aes(x = campaign, fill = y))+
  geom_bar(stat = "count")+
  labs(title = "Distribution of the contacts")

# Outcome of previous campaigns
data %>% 
  ggplot(aes(x = poutcome, fill = y))+
  geom_bar(position = "fill"
  )+
  labs(title = "Outcome of the previous campaigns",
       subtitle = "by this campaign status")

#---------------------
# Modeling 
# Sampling the data
# Sampling data for test and train sets
library(mlr)

#70% of the dataset
set.seed(1234)
smp_size <- floor(0.7*nrow(data))
set.seed(123)
train_index <- sample(seq(nrow(data)), size = smp_size)

#Assign to the train and test datasets
train <- data[train_index, ]
test <- data[-train_index, ]

# Configure classification task
classif.task <- makeClassifTask(data = train, target = 'y', id = 'bank')

# Configure tune control search and a 5-CV stratified sampling
ctrl  <- makeTuneControlGrid()
rdesc <- makeResampleDesc("CV", iters = 5L, stratify = TRUE)


# Decision tree

# Configure learners with probability type
learner1 <- makeLearner('classif.rpart', predict.type = 'prob') 

# Obtain parameters available for fine-tuning
getParamSet(learner1)


# Configure tune control search and a 5-CV stratified sampling
ctrl  <- makeTuneControlGrid()
rdesc <- makeResampleDesc("CV", iters = 5L, stratify = TRUE)

# Configure learners with probability type
learner1 <- makeLearner('classif.rpart', predict.type = 'prob') 

# Obtain parameters available for fine-tuning
getParamSet(learner1)

# Make Param Set
ps1 <- makeParamSet(
  makeDiscreteParam('maxdepth', values = c(seq(1,30,3))),
  makeDiscreteParam('minsplit', values = c(seq(1,30,3))))

# Configure tune Params settings
tunedLearner1_tuneparams <- tuneParams(learner = learner1, 
                                       task = classif.task, 
                                       resampling = rdesc,
                                       par.set = ps1, 
                                       control = ctrl,
                                       show.info =FALSE
)


# Getting the hyper parameter effects: 
learner1_effect <- generateHyperParsEffectData(tunedLearner1_tuneparams)

#Plot the effect
plotHyperParsEffect(learner1_effect, x = "iteration", y = "mmce.test.mean", plot.type = "line") + 
  ggtitle("The Hyperparameter Effects of Decision Tree")


# Making the tuned model: 
tunedLearner1 <- setHyperPars(learner1, par.vals = tunedLearner1_tuneparams$x)

# Train the tune wrappers
tunedMod1  <- train(tunedLearner1, classif.task)

# Predict on training data
tunedPred1 <- predict(tunedMod1, classif.task)

# Configure learners with probability type
learner2 <- makeLearner('classif.randomForest', predict.type = 'prob')

# Obtain parameters available for fine-tuning
getParamSet(learner2)



# Make Param Set
ps2 <- makeParamSet(
  makeDiscreteParam('mtry', values = c(2,3,4,5,6)),
  makeDiscreteParam('ntree', values = c(seq(10,100,10))
  ))

# Configure tune Params settings
tunedLearner2_tuneparams <- tuneParams(learner = learner2, 
                                       task = classif.task, 
                                       resampling = rdesc,
                                       par.set = ps2, 
                                       control = ctrl,
                                       show.info = FALSE
)

# Getting the hyper parameter effects: 
learner2_effect <- generateHyperParsEffectData(tunedLearner2_tuneparams)

#Plot the effect
plotHyperParsEffect(learner2_effect, 
                    x = "iteration", 
                    y = "mmce.test.mean", 
                    plot.type = "line") + 
  ggtitle("The Hyperparameter Effects of Random Forest")


# Making the tuned model: 
tunedLearner2 <- setHyperPars(learner2, par.vals = tunedLearner2_tuneparams$x)

# Train the tune wrappers
tunedMod2  <- train(tunedLearner2, classif.task)

# Predict on training data
tunedPred2 <- predict(tunedMod2, classif.task)

# Naive Bayes

# Configure learners with probability type
learner4 <- makeLearner('classif.naiveBayes',predict.type = 'prob')

# Obtain parameters available for fine-tuning
getParamSet(learner4)

# Make Param Set
ps4 <-makeParamSet(makeNumericParam("laplace", lower = 0, upper = 25))

# Configure tune Params settings
tunedLearner4_tuneparams <- tuneParams(learner = learner4, 
                                       task = classif.task, 
                                       resampling = rdesc,
                                       par.set = ps4, 
                                       control = ctrl,
                                       show.info = FALSE)

# Getting the hyper parameter effects: 
learner4_effect <- generateHyperParsEffectData(tunedLearner4_tuneparams)

#Plot the effect
plotHyperParsEffect(learner4_effect, 
                    x = "iteration", 
                    y = "mmce.test.mean", 
                    plot.type = "line") +  
  ggtitle("The Hyperparameter Effects of Naive Bayes")



# Making the tuned model: 
tunedLearner4 <- setHyperPars(learner4, par.vals = tunedLearner4_tuneparams$x)

# Train the tune wrappers
tunedMod4  <- train(tunedLearner4, classif.task)

# Predict on training data
tunedPred4 <- predict(tunedMod4, classif.task)


# Run SPSA on tuned Decision Tree learner
learner1_FS <- spFSR:: spFeatureSelection(classif.task, 
                                          wrapper = tunedLearner1, 
                                          measure = mmce, 
                                          num.features.selected = 0, 
                                          show.info = FALSE)

# Get the best models from feature selection
spsaModel <- learner1_FS$best.model

#Plot the important variables
spFSR::plotImportance(learner1_FS)


# Treshold adjustment

# Generate data on threshold vs. performance(s) and
d1 <- generateThreshVsPerfData(tunedPred1, measures = list(mmce))

# Plot the threshold adjustment
plotThreshVsPerf(d1) + labs(title = 'Threshold Adjustment for Decision Tree', x = 'Threshold')


# Get threshold value
threshold1 <- d1$data$threshold[ which.min(d1$data$mmce) ]

# Generate data on threshold vs. performance(s) and
d2 <- generateThreshVsPerfData(tunedPred2, measures = list(mmce))

# Plot the threshold adjustment
plotThreshVsPerf(d2) + labs(title = 'Threshold Adjustment for Random Forest', x = 'Threshold')

# Get threshold value
threshold2 <- d2$data$threshold[ which.min(d2$data$mmce) ]

# Generate data on threshold vs. performance(s)
d4 <- generateThreshVsPerfData(tunedPred4, measures = list(mmce))

# Plot the threshold adjustment
plotThreshVsPerf(d4) + labs(title = 'Threshold Adjustment for Naive Bayes', x = 'Threshold')

# Get threshold value
threshold4 <- d4$data$threshold[ which.min(d4$data$mmce) ]

# Decision Tree
testPred1 <- predict(tunedMod1, newdata = test)
testPred1 <- setThreshold(testPred1, threshold1 )
# Random Forest
testPred2 <- predict(tunedMod2, newdata = test)
testPred2 <- setThreshold(testPred2, threshold2 )
# Naive Bayes
testPred4 <- predict(tunedMod4, newdata = test)
testPred4 <- setThreshold(testPred4, threshold4 )
# Decision Tree with Feature Selection
testPred1_FS <- predict(spsaModel, newdata = test)


# AUC Curves

#Comparing Decision Tree performance with Random Forest and Naive Bayes by using `plotROCCurves` plots
compare1 <- generateThreshVsPerfData(list(DecisionTree = testPred1, randomForest = testPred2, naivebayes = testPred4, DecisionTree_FS = testPred1_FS), 
                                     measures = list(fpr, tpr))

plotROCCurves(compare1)


## Paired t-test

# Configure classification task for test data
classif.task_test <- makeClassifTask(data = test, target = 'y', id = 'bank')
#Perform benchmark on each learner
bmr <- benchmark(learners = list(
  makeLearner('classif.rpart', predict.type = 'prob'),
  makeLearner('classif.randomForest', predict.type = 'prob'),
  makeLearner('classif.naiveBayes',predict.type = 'prob')
), classif.task_test, rdesc, measures = auc)


#Get the overal performance
performance <- getBMRPerformances(bmr, as.df = TRUE)

#Subset the data frame for Decision Tree
performance_rpart <- performance[c(1:5),]
#Subset the data frame for Random Forest
performance_rf <- performance[c(6:10),]
#Subset the data frame for Naive Bayes
performance_nb <- performance[c(11:15),]
# t-test for Decision Tree and Random Forest
t.test(performance_rpart$auc, performance_rf$auc, paired = TRUE, alternative = "two.sided")


# t-test for Decision Tree and Naive Bayes
t.test(performance_rpart$auc, performance_nb$auc, paired = TRUE, alternative = "two.sided")


# t-test for Random Forest and Naive Bayes
t.test(performance_rf$auc, performance_nb$auc, paired = TRUE, alternative = "two.sided")

# Calculate the confusion matrix for Decision Tree
calculateConfusionMatrix( testPred1,relative = TRUE)

performance(testPred1, measures = list(f1, tpr, tnr, fpr, fnr, mmce))



# Calculate the confusion matrix for Random Forest
calculateConfusionMatrix( testPred2,relative = TRUE)

performance(testPred2, measures = list(f1, tpr, tnr, fpr, fnr, mmce))


# Calculate the confusion matrix for Naive Bayes
calculateConfusionMatrix( testPred4,relative = TRUE)

performance(testPred4, measures = list(f1, tpr, tnr, fpr, fnr, mmce))


# Calculate the confusion matrix for Decision Tree with Feature Selection
calculateConfusionMatrix(testPred1_FS,relative = TRUE)



performance( testPred1_FS )







