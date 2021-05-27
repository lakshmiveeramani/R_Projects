################################################################
# Project: Stroke Prediction                                   #
# Author: Ying Mi                                              #
# Description: Codes for data loading, exploration, analysis,  #
#              modeling and prediction                         #
################################################################


#######################################################
# Install packages for library used in stroke project #
#######################################################

# List regular packages that are used in stroke project
list.of.packages <-c("dplyr","readr","Hmisc","descriptr","AMR","tidyverse",
                     "ggplot2","ggpubr","gridExtra","ggrepel","ggmosaic","caret",
                     "randomForest","class"
                     )
# check any packages not in local machine
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
# install any package if it is not in local machine
if(length(new.packages)) install.packages(new.packages)

# List special package that is used in stroke project
specialpackage <-c("DMwR")
# check whether special packages is installed in local machine
spec.packages <- list.of.packages[!(specialpackage %in% installed.packages()[,"Package"])]
# If it isn't installed in local machine, install it
if(length(spec.packages)) 
install.packages("https://cran.r-project.org/src/contrib/Archive/DMwR//DMwR_0.4.1.tar.gz", 
                 repos=NULL, type="source")

#########################################
# Data loading in R and data quick view #
#########################################

library(dplyr)
library (readr)
# Data file is located in github
urlfile="https://raw.githubusercontent.com/YingMm/Stroke/master/healthcare-dataset-stroke-data.csv"
# Download file and save it named as data.csv in local machine working directory
tmp_filename <-file.path(getwd(),"data.csv")
download.file(urlfile, tmp_filename)
# Load data from local file 
dat <- read_csv(tmp_filename)
# Check data dimension for initial dataset
options(warn=-1)
knitr::kable(t(dim(dat)),caption = "Initial dataset dimension")
# Check basic descriptive statistic for initial dataset
library(Hmisc)
print(describe(dat))

#################
# Data Cleaning #
#################

##################################### 
# a. Replace missing values with NA #
#####################################
dat[dat=='N/A']<-NA
dat[dat=='Unknown']<-NA

##########################################################################
# b. Transform data type for categorical features & outcome to factor,   #
#    quantitative feature to numeric for data exploration and analysis.  #
##########################################################################

library(descriptr)
datAdj<- data.frame(dat) %>% 
  mutate(
    id=as.numeric(id),
    gender=factor(gender),
    age=as.numeric(age),
    hypertension=factor(ifelse(hypertension==1, "Yes","No")), 
    heart_disease=factor(ifelse(heart_disease==1,"Yes","No")),
    ever_married=factor(ever_married), 
    work_type=factor(work_type),
    Residence_type=factor(Residence_type),
    avg_glucose_level=as.numeric(avg_glucose_level),
    bmi=as.numeric(bmi),
    smoking_status=factor(smoking_status),
    stroke=factor(as.numeric(stroke)))

##################################
# c. Dealing with missing values #
##################################

########################
# Recode missing value #
########################

######################
# BMI missing values #
######################

library(AMR)
# Split ages into age groups by every 5 years 
datAdj<-datAdj %>%mutate(agegroup=age_groups(age, split_at = "fives"))
# Get average BMI values by gender and age group
datAdj<-datAdj %>%group_by(gender,agegroup) %>%mutate(bmi_avg=mean(bmi,na.rm=T))
# replace BMI missing value with average BMI value with same gender and age group
datAdj$bmi[is.na(datAdj$bmi)]  <- datAdj$bmi_avg[is.na(datAdj$bmi)]

#################################
# smoking status missing values #
#################################

# Replace smoking_status with 'never smoked' for patient age <15
datAdj$smoking_status[datAdj$age<15]<-"never smoked"
# Use most frequent non-NA value for smoking status in same gender and age group 
# to replace missing value for patient age >=15
datAdj<-datAdj %>%
  group_by(gender,agegroup) %>%
  add_count(smoking_status) %>%
  mutate(smoking_status= if_else(is.na(smoking_status), 
                                 smoking_status[which.max(n)], 
                                 smoking_status)) %>%
  select(-n) %>%
  ungroup()

# Check dataset again for missing values stats after adjustments
print(ds_screener(datAdj %>% select(-c("bmi_avg","agegroup"))))

##########################################################
# Exclude the rest of missing values that can't be coded #
##########################################################
# Exclude NA observation and remove temporary columns and structure dataset
datFin <-na.omit(datAdj)%>% select(-c("bmi_avg","agegroup"));datFin <-data.frame(datFin)

###################################
# d. Remove outliers from dataset #
###################################

# Remove observation that gender is other 
datFin<- datFin %>% filter(gender!='Other')

# Quick view of dataset for modelling
set.seed(1, sample.kind="Rounding") 
knitr::kable(datFin %>%group_by(stroke) %>%sample_n(3)%>%t(),
             caption = "Random 6 rows of dataset for modeling")

####################
# Data Exploration #
####################

library(tidyverse)
library(ggplot2)
library(ggpubr)

#####################################
# a. Counts of categorical features #
#####################################

#visualized all categorical feature's counts
datFreqExpCat<-gather(subset(datFin, select = -c(age,avg_glucose_level,bmi,stroke)),
                      "Attribute","Level",-id)  
datFreqExpCat%>% 
  ggplot(aes(x = Level)) +
  theme_pubclean()+
  geom_bar(fill = "lightskyblue") +geom_text(stat='Count',aes(label=..count..),
                                             colour="midnightblue",
                                             vjust = "inward",
                                             size=3)+
  facet_wrap(vars(Attribute),scales = "free_x", ncol =1)+
  theme_bw()+
  theme(strip.background =element_rect(fill="navy"))+
  theme(strip.text = element_text(colour = 'white',size = 10, face='bold' ))

###########################################
# b. Distribution of continuous variables #
###########################################

# Visualize distribution of age and explore median values for age
p1<-ggplot(datFin, aes(x = age)) + 
  geom_histogram(bins = 30, color = "grey17", fill = "lightskyblue") +
  theme_pubclean()+
  geom_vline(aes(xintercept = median(age)), 
             linetype = "dashed", size = 0.6)+
  geom_text(aes(x=median(age)+3, label=paste("Median:", round(median(age),digit=0)),
                y=80), colour="midnightblue") +coord_flip()

# Visualize distribution of avg_glucose_level and explore median values for avg_glucose_level
p2<-ggplot(datFin, aes(x = avg_glucose_level)) + 
  geom_histogram(bins = 30, color = "grey17", fill = "lightskyblue") +
  theme_pubclean()+
  geom_vline(aes(xintercept = median(avg_glucose_level)), 
             linetype = "dashed", size = 0.6)+
  geom_text(aes(x=median(avg_glucose_level)+8, 
                label=paste("Median:", round(median(avg_glucose_level),digit=0))
                , y=210), colour="midnightblue")  +coord_flip()

# Visualize distribution of bmi and explore median values for bmi
p3<-ggplot(datFin, aes(x = bmi)) + 
  geom_histogram(bins = 30, color = "grey17", fill = "lightskyblue") +
  theme_pubclean()+
  geom_vline(aes(xintercept = median(bmi)), 
             linetype = "dashed", size = 0.6)+
  geom_text(aes(x=median(bmi)+3, 
                label=paste("Median:", round(median(bmi),digit=1))
                , y=300), colour="midnightblue")  +coord_flip()

# Bring 3 distribution graphs together in one row
library(gridExtra)
grid.arrange(p1, p2, p3, nrow = 1)

##############################
# c. Distribution of outcome #
##############################

# Pie chart for stroke's distribution
library(ggrepel)
datFin %>% 
  group_by(stroke) %>% 
  summarise(n = n()) %>% 
  mutate(prop = n / sum(n)) %>% mutate(label=paste0(round(prop*100,1),'%'), 
                                       cumulative = cumsum(n),
                                       midpoint = cumulative+ n / 2)%>% 
  ggplot(aes(x ="", n, fill = stroke)) +
  geom_col(color="white") +
  coord_polar(theta = "y", start=0) +
  geom_label_repel(aes(label = label), size=5, show.legend = F, nudge_x = 1) + 
  theme_void()

#############################################################
# d. Distribution of categorical features vs stroke outcome #
#############################################################

library(ggmosaic)
# Explore stroke distribution by gender
mosaicsg<-ggplot(data = datFin) +
  geom_mosaic(aes(x = product(stroke, gender), fill=stroke))+
  theme_bw()+ 
  guides(fill=FALSE)
# Explore stroke distribution by hypertension
mosaicshy<-ggplot(data = datFin) +
  geom_mosaic(aes(x = product(stroke, hypertension), fill=stroke))+
  theme_bw()+ 
  guides(fill=FALSE)
# Explore stroke distribution by heart_disease
mosaicsh<-ggplot(data = datFin) +
  geom_mosaic(aes(x = product(stroke, heart_disease), fill=stroke))+
  theme_bw()+ 
  guides(fill=FALSE)
# Explore stroke distribution by ever_married
mosaicsm<-ggplot(data = datFin) +
  geom_mosaic(aes(x = product(stroke, ever_married), fill=stroke))+
  theme_bw()+ 
  guides(fill=FALSE)
# Explore stroke distribution by work_type
mosaicsw<-ggplot(data = datFin) +
  geom_mosaic(aes(x = product(stroke, work_type), fill=stroke))+
  theme_bw()+
  theme(legend.position="top")
# Explore stroke distribution by Residence_type
mosaicsr<-ggplot(data = datFin) +
  geom_mosaic(aes(x = product(stroke, Residence_type), fill=stroke))+
  theme_bw()+ 
  guides(fill=FALSE)
# Explore stroke distribution by smoking_status
mosaicss<-ggplot(data = datFin) +
  geom_mosaic(aes(x = product(stroke, smoking_status), fill=stroke))+
  theme_bw()+ 
  guides(fill=FALSE)

# Bring distribution graphs together
grid.arrange(mosaicsw,mosaicsg,mosaicshy, mosaicsh,mosaicsm,mosaicsr,mosaicss,ncol = 2,
             layout_matrix = rbind(c(1, 1),
                                   c(2, 3),
                                   c(4, 5),
                                   c(6, 7)
             )
)

########################################################
# e. Density of continuous variables vs stroke outcome #
########################################################
# Explore density by stroke and age
p1s<-ggplot(datFin, aes(x = age,fill = stroke)) + 
  geom_density(alpha = 0.5) +
  theme_pubclean()+theme(legend.position="left")
# Explore density by stroke and avg_glucose_level
p2s<-ggplot(datFin, aes(x = avg_glucose_level,fill = stroke)) + 
  geom_density(alpha = 0.5) + guides(fill=FALSE)+
  theme_pubclean()
# Explore density by stroke and bmi
p3s<-ggplot(datFin, aes(x = bmi,fill = stroke)) + 
  geom_density(alpha = 0.5) + guides(fill=FALSE)+
  theme_pubclean()
# Bring distribution graphs together in one row
grid.arrange(p1s, p2s, p3s, nrow = 1)


###########
# Results #
###########


library(caret)
################################################################################
# 1. Finalize dataset for modeling, split train and test data set for modeling #
################################################################################

# Remove id column
datFin=subset(datFin, select = -c(id) )
# Adjust outcome to integer data type
datFin$stroke<- as.integer(as.character(datFin$stroke))
# Adjust factor features to numeric features
datFin<-mutate_if(datFin, is.factor, ~ as.numeric((.x)))

# Using 90% of final dataset as train dataset, 10% of final dataset as validation dataset.
set.seed(123, sample.kind="Rounding") 
Validation_index <- createDataPartition(y = datFin$stroke, times = 1, p = 0.1, list = FALSE)
Train <- datFin[-Validation_index,]
Validation <- datFin[Validation_index,]

###########################################################
# 2. Use Logistic regression model to predict stroke in R #
###########################################################
# Build GLM model with binary outcome (i.e. Logistic regression model) through train dataset
LogisticModel <- glm(as.factor(stroke) ~ gender+age+hypertension+heart_disease+ever_married+
                       work_type+avg_glucose_level+bmi+smoking_status+Residence_type,
                     data =Train, family = "binomial")
# Calculate the predicted probabilities in the form of P(y=1|x) at validation dataset
LogisticProbabilities <- LogisticModel %>% predict(Validation, type = "response")
# If P(y=1|x) > 0.5 then y = 1 otherwise y=0.
Logisticpredicted <- ifelse(LogisticProbabilities > 0.5,1, 0)
# Using confusionMatix to check results
LogisticFin<-confusionMatrix(as.factor(Logisticpredicted), 
                             as.factor(Validation$stroke),  
                             positive = "1")

print(LogisticFin)

##########################################################
# 3. Adjust unbalance train dataset using SMOTE way in R #
##########################################################

# Explore proportion and counts of stroke outcome for train dataset
TrainStrokepropInitial <- Train %>%
  count(stroke) %>%            # summarise counts
  mutate(prop = prop.table(n))    # summarise proportion

knitr::kable(as.data.frame(TrainStrokepropInitial),
             caption = "Train dataset stroke outcome cases & proportion before adjustment")


# Using SMOTE way to adjust unblanced train dataset
library(DMwR)
Train$stroke<-as.factor(Train$stroke)
trainSOMTE <- SMOTE(stroke ~ ., Train, perc.over = 100, perc.under=200)

# Explore proportion and counts of stroke outcome for train dataset after adjustments
TrainStrokepropAdj <- trainSOMTE %>%
  count(stroke) %>%            # summarise counts
  mutate(prop = prop.table(n))    # summarise proportion

knitr::kable(as.data.frame(TrainStrokepropAdj),
             caption = "Train dataset stroke outcome cases & proportion after adjustment")

#############################################################################################
# 4a. Apply Logistic regression model to balanced train dataset and rebuild data model in R #
#############################################################################################


trainSOMTE$stroke<-as.integer(as.character(trainSOMTE$stroke))
set.seed(123) 

# Set up 10-fold cross validation in train dataset to prevent Overfitting 
train.control <- trainControl(method = "cv", number = 10)
# Apply GLM model with binary outcome (i.e. Logistic regression model) to train dataset
# with 10-fold cross validation
SmoteLogisticModelmodel <- train(form = factor(stroke)~gender+age+hypertension+
                                   heart_disease+ever_married+
                                   work_type+avg_glucose_level+bmi+smoking_status+
                                   Residence_type, data = trainSOMTE,
                                 method = "glm",family = "binomial",
                                 trControl = train.control)
# Explore GLM model with binary outcome
SmoteLogisticModelmodel
# Calculate the predicted probabilities at validation dataset
SmoteLogisticProbabilities <- SmoteLogisticModelmodel%>% predict(Validation, type = "prob")
# Look at stroke case outcome, if predicted probabilities > 0.5 then stroke case otherwise un-stroke case
SmoteLogisticpredicted <- ifelse(SmoteLogisticProbabilities[,"1"] > 0.5,1, 0)
# Using confusionMatix to check results
SmoteLogisticFin<-confusionMatrix(relevel(factor(SmoteLogisticpredicted),ref ="1"), 
                                  relevel(factor(Validation$stroke), ref ="1"), 
                                  positive = "1")
print(SmoteLogisticFin)

#####################################################################################
# 4b. Apply Random forest model to balanced train dataset and build data model in R #
#####################################################################################

library(randomForest)
# Apply Random forest model to train dataset with 10-fold cross validation
# Tune parameter for Number of variables available for splitting at each tree node. 

tunegrid <-expand.grid(.mtry=seq(1,10,1))

SmoteRandomforestModel <- train(form = factor(stroke)~gender+age+hypertension+
                                  heart_disease+ever_married+
                                  work_type+avg_glucose_level+bmi+
                                  smoking_status+Residence_type, data = trainSOMTE,
                                method = "rf",
                                tuneGrid=tunegrid,
                                trControl = train.control)
# Explore Random forest model
SmoteRandomforestModel

# Calculate the predicted probabilities at validation dataset
SmoteRandomforestProbabilities <- SmoteRandomforestModel %>% 
  predict(Validation, type = "prob")

# Look at stroke case outcome, if predicted probabilities > 0.5 then stroke case otherwise un-stroke case
SmoteRandomforestpredicted <- ifelse(SmoteRandomforestProbabilities[,"1"] > 0.5
                                     ,1, 0)
# Using confusionMatix to check results
SmoteRandomforestFin<-confusionMatrix(relevel(factor(SmoteRandomforestpredicted),ref ="1"),
                                      relevel(factor(Validation$stroke),ref ="1"), 
                                      positive = "1")

SmoteRandomforestFin

###########################################################################
# 4c. Apply KNN model to balanced train dataset and build data model in R #
###########################################################################


library(class)

# Adjust dataset to fit KNN model
# create feature dataset for training model
knnTrain<-subset(trainSOMTE, select = -c(stroke) )
# create feature dataset for validating model
knnValidation<-subset(Validation, select = -c(stroke) )

# Apply KNN model to train dataset with 10-fold cross validation
SmoteknnModel <- train(knnTrain,factor(trainSOMTE$stroke), method = "knn",
                       tuneLength = 10,
                       trControl = train.control)
# Explore Random forest model
SmoteknnModel 

# Calculate the predicted probabilities at validation dataset
SmoteknnProbabilities <- SmoteknnModel %>% predict(Validation, type = "prob")

# Look at stroke case outcome, if predicted probabilities > 0.5 then stroke case otherwise un-stroke case
Smoteknnpredicted <- ifelse(SmoteknnProbabilities[,"1"] > 0.5,1, 0)

# Using confusionMatix to check results
SmoteknnFin <-confusionMatrix(relevel(factor(Smoteknnpredicted),ref ="1"), 
                              relevel(factor(Validation$stroke),ref ="1"), 
                              positive = "1")

SmoteknnFin

############################
# 5.Comparison of 3 models #
############################

# Build function to visualize confusion matrix table
plot_confusion_matrix <- function(Tb,Tl) {
  
  ConMT= Tb
  ConMT<-ConMT %>% mutate(colorFlag=ifelse(Prediction==Reference,1,0))
  
  ComMTPlot= ggplot(data =  ConMT,
                    mapping = aes(x = Reference , y = Prediction )) +
    geom_tile(aes(fill = colorFlag), colour = "white") +
    geom_text(aes(label = sprintf("%1.0f", Freq)), vjust = 1) +
    scale_fill_gradient(low="peachpuff2",high = "seagreen2") +
    theme_bw() + theme(legend.position = "none")+
    theme(axis.text.x = element_text(colour="black",size=10))+
    theme(axis.text.y = element_text(colour="black",size=10))+
    theme(text = element_text(size=10,colour="black"))+
    labs(title=Tl)
}


# visualize confusion matrix tables for 3 models
p1c<-plot_confusion_matrix(as.data.frame(SmoteLogisticFin$table),"Logistic regression")
p2c<-plot_confusion_matrix(as.data.frame(SmoteRandomforestFin$table),"Random Forest")
p3c<-plot_confusion_matrix(as.data.frame(SmoteknnFin$table),"KNN")
grid.arrange(p1c, p2c, p3c, nrow = 1)


# Build function to get each model's accuracy, F1, sensitivity, specificity
metric_confusion_matrix <- function(cmm,Tl) {
  
  M<-data_frame(
    Model=Tl,Metric=names(cmm$overall[1]),Value=as.numeric(cmm$overall[1])
  )
  M <- bind_rows(M,data_frame(Model=Tl,Metric=names(cmm$byClass[1]),
                              Value=as.numeric(cmm$byClass[1])))
  M <- bind_rows(M,data_frame(Model=Tl,Metric=names(cmm$byClass[2]),
                              Value=as.numeric(cmm$byClass[2])))
  M <- bind_rows(M,data_frame(Model=Tl,Metric=names(cmm$byClass[7]),
                              Value=as.numeric(cmm$byClass[7])))
}

# Get each model's accuracy, F1, sensitivity, specificity
p1m<-metric_confusion_matrix(SmoteLogisticFin,"Logistic regression") 
p2m<-metric_confusion_matrix(SmoteRandomforestFin,"Random Forest") 
p3m<-metric_confusion_matrix(SmoteknnFin,"KNN") 

# Combine 3 model's results into one table
pm<-bind_rows(p1m,p2m,p3m)
pmprint<-pm %>%spread(Metric, Value)
knitr::kable(pmprint)
options(warn=0)

