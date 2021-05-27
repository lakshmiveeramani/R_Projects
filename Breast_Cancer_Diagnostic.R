# =========== Section 1 : Packages and libraries ===========
# Install (if required) and open required package and libraries
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(readr)) install.packages("readr", repos = "http://cran.us.r-project.org")
if(!require(corrplot)) install.packages("corrplot", repos = "http://cran.us.r-project.org")
if(!require(RColorBrewer)) install.packages("RColorBrewer", repos = "http://cran.us.r-project.org")
if(!require(kableExtra)) install.packages("kableExtra", repos = "http://cran.us.r-project.org")
if(!require(fpc)) install.packages("fpc", repos = "http://cran.us.r-project.org")
if(!require(utils)) install.packages("utils", repos = "http://cran.us.r-project.org")
if(!require(klaR)) install.packages("klaR", repos = "http://cran.us.r-project.org")
if(!require(grid)) install.packages("grid", repos = "http://cran.us.r-project.org")
if(!require(ggfortify)) install.packages("ggfortify", repos = "http://cran.us.r-project.org")
if(!require(glmnet)) install.packages("glmnet", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org")
if (!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(scales)) install.packages("scales", repos = "http://cran.us.r-project.org")

library(tidyverse)
library(dplyr)
library(ggplot2)
library(knitr)
library(caret)
library(data.table)
library(lubridate)
library(caret)
library(readr)
library(corrplot)
library(RColorBrewer)
library(kableExtra)
library(utils)
library(fpc)
library(factoextra)
library(matrixStats)
library(scales)

# =========== Section 2 : Data download and preparation ===========
# Breast Cancer Wisconsin (Diagnostic) Data Set
# http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data
# Data Set Description
# https://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.names

# Download data
data_url <- "http://archive.ics.uci.edu/ml/machine-learning-databases/breast-cancer-wisconsin/wdbc.data"

# download data and create data.frame
wdbc_data <- fread(data_url)

# add column names (based on WPBC.names)
col_names <- c("id","diagnosis","radius_mean","texture_mean", "perimeter_mean",
               "area_mean","smoothness_mean","compactness_mean","concavity_mean",
               "concave_points_mean", "symmetry_mean","fractal_dimension_mean",
               "radius_se","texture_se", "perimeter_se","area_se","smoothness_se",
               "compactness_se", "concavity_se","concave_points_se","symmetry_se",
               "fractal_dimension_se", "radius_worst","texture_worst",
               "perimeter_worst", "area_worst","smoothness_worst",
               "compactness_worst", "concavity_worst","concave_points_worst",
               "symmetry_worst", "fractal_dimension_worst")

colnames(wdbc_data) <- col_names

#check data download
str(wdbc_data)

# =========== Section 3 : Data exploration ===========
# Check if there are missing values in the wdbc_data set
if (any(is.na(wdbc_data))) {
  print("They are as missing values in the data set ")
} else {
  print("They are no missing values in the data set")
}

# wdbc_data head - transposed for easier display
t(head(wdbc_data, n = 5))

# Diagnosis distribution
wdbc_data %>% ggplot(aes(diagnosis)) + 
  geom_bar(fill = "#006699", color = "#ffffff") +
  ggtitle("Diagnosis distribution") +
  xlab("Diagnosis") +
  ylab("Number of diagnosis") +
  theme_bw(base_size = 11)

# wdbc_data summary - transposed for easier display
t(summary(wdbc_data[,3:32]))

# Create table with mean scores of each features - transposed for easier display
t(summary(wdbc_data[,3:12]))

# Create table with standard error scores of each features - transposed for easier display
t(summary(wdbc_data[,13:22]))

# Create table with worst scores of each features - transposed for easier display
t(summary(wdbc_data[,23:32]))
wdbc_data
# Plot and facet wrap density plots for each feature - quick distribution overview

wdbc_data %>% dplyr::select(-id, -diagnosis) %>%
  gather("feature", "value") %>%
  ggplot(aes(value)) +
  geom_density(fill = "#006699", alpha = 1) +
  facet_wrap(~ feature, scales = "free", ncol = 3) +
  ggtitle("Density plots for each features") +
  xlab("Feature values") +
  ylab("Density") +
  theme(legend.position = "bottom", 
        panel.background = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        strip.text = element_text(size=8))

# Plot and facet wrap density plots for each feature by diagnosis
wdbc_data %>% dplyr::select(-id) %>%
  gather("feature", "value", -diagnosis) %>%
  ggplot(aes(value, fill = diagnosis)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ feature, scales = "free", ncol = 3) +
  scale_fill_manual( values = c("#993300","#006699")) +
  ggtitle("Density plots by diagnosis for each features") +
  xlab("Feature values") +
  ylab("Density") +
  scale_fill_discrete(labels = c("Benign", "Malignant")) +
  theme(legend.position = "bottom", 
        panel.background = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        strip.text = element_text(size=8))

# Plot and facet wrap density plots for mean scores of each features by diagnosis
wdbc_data %>% dplyr::select(-id) %>%
  gather("feature", "value", -diagnosis) %>%
  ggplot(aes(value, fill = diagnosis)) +
  geom_density(alpha = 0.6) +
  facet_wrap(~ feature, scales = "free", ncol = 3) +
  scale_fill_manual( values = c("#993300","#006699")) +
  ggtitle("Density plots by diagnosis for each features") +
  xlab("Feature values") +
  ylab("Density") +
  scale_fill_discrete(labels = c("Benin", "Malignant")) +
  theme(legend.position = "bottom", 
        panel.background = element_blank(), 
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(),
        legend.title=element_blank(),
        strip.text = element_text(size=8))

# Plot correlation between features
# Features correlation for begin diagnostics
benin <- wdbc_data %>% filter(diagnosis == "B")
corrplot(cor(benin[,3:32]), method = "circle", order = "hclust", na.label = "NA", tl.cex = 0.6, tl.col = "black", tl.srt = 90, col = brewer.pal(n = 10, name = "RdBu"))

# Features correlation for malignant diagnostics
malignant <- wdbc_data %>% filter(diagnosis == "M")
corrplot(cor(malignant[,3:32]), method = "circle", order = "hclust", na.label = "NA", tl.cex = 0.6, tl.col = "black", tl.srt = 90, col = brewer.pal(n = 10, name = "RdBu"))

# =========== Section 4 : Data Preparation ===========
# Convert diagnosis to a factor for model set
wdbc_data$diagnosis <- as.factor(wdbc_data$diagnosis)

# combine features and diagnosis into list object #
brca <- list(x = wdbc_data[,3:32] %>% as.matrix(), y = wdbc_data$diagnosis)

# =========== Section 5 : Data Analysis ===========
## Principal Component Analysis (PCA) ##

# Simple principal components analysis plot
plot(prcomp(wdbc_data[,3:32], center = TRUE, scale = TRUE), title(main = NULL), type="l")

# Store PCA result in a pca object   
wdbc_pca <- prcomp(wdbc_data[,3:32], center = TRUE, scale = TRUE)

# Table showing components
summary(wdbc_pca)$importance

# Create data frame with components importance
pca_importance <- data.frame(summary(wdbc_pca)$importance)

# Plot % of explained variance by components
data.frame(variance = summary(wdbc_pca)$importance[2,]) %>% rownames_to_column("PCA") %>% 
  ggplot(aes(x = reorder(PCA, -variance), y = variance)) + 
  geom_bar(stat = "identity", fill = "#006699", color = "#ffffff") +
  ggtitle("Variance explained by components") +
  xlab("Components") +
  ylab("% of variance Explained") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# Plot cumulative variance with line at 95%
data.frame(variance = summary(wdbc_pca)$importance[3,]) %>% rownames_to_column("PCA") %>% 
  ggplot( aes(reorder(PCA, variance), variance)) + 
  geom_point(color = "#006699") +
  ggtitle("Cumulative variance explained by components") +
  xlab("Components") +
  ylab("% of variance Explained") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_hline(aes(yintercept = 0.95), color = "#993300", linetype = 2) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# Table showing 10 most important components
summary(wdbc_pca)$importance[,1:10]

# Create data frame with 10 most important components
pca_df <- as.data.frame(wdbc_pca$x) 

# Boxplot of 10 most important PC by diagnosis
data.frame(wdbc_pca$x[,1:10], Diagnosis = brca$y) %>%
  gather(key = "PC", value = "value", -Diagnosis) %>%
  ggplot(aes(PC, value, fill = Diagnosis)) +
  geom_boxplot() +
  scale_fill_manual(name="Diagnosis",
                      breaks=c("B", "M"),
                      labels=c("Benign", "Malignant")
                      ,values = c("#006699", "#993300")) +
  ggtitle("10 most important PC by diagnosis") +
  theme_bw() +
  theme(legend.position = "bottom")

# Scatterplot of PC1 versus PC2)
data.frame(wdbc_pca$x[,1:2], Diagnosis = brca$y) %>%
  ggplot(aes(PC1, PC2, color = Diagnosis)) +
  geom_point(alpha = 0.6) +
  stat_ellipse() +
  xlab("PC1") +
  ylab("PC2") +
  scale_color_manual(name="Diagnosis", labels=c("Benign", "Malignant"),values = c("#006699", "#993300"))

# Scatterplot of PC3 versus PC4)
data.frame(wdbc_pca$x[,3:4], Diagnosis = brca$y) %>%
  ggplot(aes(PC3, PC4, color = Diagnosis)) +
  geom_point(alpha = 0.6) +
  stat_ellipse() +
  xlab("PC1") +
  ylab("PC2") +
  scale_color_manual(name="Diagnosis", labels=c("Benign", "Malignant"),values = c("#006699", "#993300"))


# =========== Section 6 : Model Building  ===========
# scale x values 
x_centered <- sweep(brca$x, 2, colMeans(brca$x))
x_scaled <- sweep(x_centered, 2, colSds(brca$x), FUN = "/")

# split brca$x and brca$y into 20% test and 80% training sets
set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`
train_index <- createDataPartition(brca$y, times = 1, p = 0.2, list = FALSE)
wdbc_train <- list(x = x_scaled[-train_index,], y= brca$y[-train_index])
wdbc_test <- list(x = x_scaled[train_index,], y= brca$y[train_index])

# check diagnostic distribution for both sets
prop.table(table(wdbc_train$y))
prop.table(table(wdbc_test$y))

# remove test_index
rm(train_index)

## logistic regression model ##
# Train logistic regression model
train_glm <- train(wdbc_train$x, wdbc_train$y, method = "glm")
# Generate regression predictions
glm_preds <- predict(train_glm, wdbc_test$x)
# Display logistic regression performance 
confusionMatrix(data = glm_preds, reference = wdbc_test$y, positive = "M")$byClass

## loess regression model ##
# Train loess model
train_loess <- train(wdbc_train$x, wdbc_train$y, method = "gamLoess")
# Generate loess predictions
loess_preds <- predict(train_loess, wdbc_test$x)
# Display loess performance
confusionMatrix(data = loess_preds, reference = wdbc_test$y, positive = "M")$byClass

## Linear Discriminant Analysis (LDA) model ##
# Train LDA model
train_lda <- train(wdbc_train$x, wdbc_train$y, method = "lda")
# Generate LDA predictions
lda_preds <- predict(train_lda, wdbc_test$x)
# Display LDA performance
confusionMatrix(data = lda_preds, reference = wdbc_test$y, positive = "M")$byClass

## Quadratic Discriminant Analysis (QDA) model ##
# Train QDA model
train_qda <- train(wdbc_train$x, wdbc_train$y, method = "qda")
# Generate QDA predictions
qda_preds <- predict(train_qda, wdbc_test$x)
# Display QDA performance
confusionMatrix(data = qda_preds, reference = wdbc_test$y, positive = "M")$byClass

## kNN model ##
# Train kNN model
train_knn <- train(wdbc_train$x, wdbc_train$y, method = "knn", tuneGrid = data.frame(k = seq(3, 29, 2)))
# Best k value
train_knn$bestTune
# Generate kNN predictions
knn_preds <- predict(train_knn, wdbc_test$x)
# Display kNN performance
confusionMatrix(data = knn_preds, reference = wdbc_test$y, positive = "M")$byClass

## Random forest model ##
# Train random forest model, find best mtry, generate predictions, measure accuracy
# Pick the mtry value
tuning <- data.frame(mtry = seq(3, 29, 2))
train_rf <- train(wdbc_train$x, wdbc_train$y, method = "rf", tuneGrid = tuning, importance = TRUE)
# Best mtry value
train_rf$bestTune
# Generate random forest prediction
rf_preds <- predict(train_rf, wdbc_test$x)
# Display random forest performance
confusionMatrix(data = rf_preds, reference = wdbc_test$y, positive = "M")$byClass

## Create data frame with models performance ##
# Model names
models <- c("Logistic regression", "Loess", "LDA", "QDA", "K nearest neighbors", "Random forest")

# Models accuracy
accuracy <- c(confusionMatrix(data = glm_preds, reference = wdbc_test$y, positive = "M")$overall["Accuracy"],
               confusionMatrix(data = loess_preds, reference = wdbc_test$y, positive = "M")$overall["Accuracy"],
               confusionMatrix(data = lda_preds, reference = wdbc_test$y, positive = "M")$overall["Accuracy"],
               confusionMatrix(data = qda_preds, reference = wdbc_test$y, positive = "M")$overall["Accuracy"],
               confusionMatrix(data = knn_preds, reference = wdbc_test$y, positive = "M")$overall["Accuracy"],
               confusionMatrix(data = rf_preds, reference = wdbc_test$y, positive = "M")$overall["Accuracy"])

# Models sensitivity
sensitivity <- c(confusionMatrix(data = glm_preds, reference = wdbc_test$y, positive = "M")$byClass["Sensitivity"],
           confusionMatrix(data = loess_preds, reference = wdbc_test$y, positive = "M")$byClass["Sensitivity"],
           confusionMatrix(data = lda_preds, reference = wdbc_test$y, positive = "M")$byClass["Sensitivity"],
           confusionMatrix(data = qda_preds, reference = wdbc_test$y, positive = "M")$byClass["Sensitivity"],
           confusionMatrix(data = knn_preds, reference = wdbc_test$y, positive = "M")$byClass["Sensitivity"],
           confusionMatrix(data = rf_preds, reference = wdbc_test$y, positive = "M")$byClass["Sensitivity"])

# Models specificity
specificity <- c(confusionMatrix(data = glm_preds, reference = wdbc_test$y, positive = "M")$byClass["Specificity"],
             confusionMatrix(data = loess_preds, reference = wdbc_test$y, positive = "M")$byClass["Specificity"],
             confusionMatrix(data = lda_preds, reference = wdbc_test$y, positive = "M")$byClass["Specificity"],
             confusionMatrix(data = qda_preds, reference = wdbc_test$y, positive = "M")$byClass["Specificity"],
             confusionMatrix(data = knn_preds, reference = wdbc_test$y, positive = "M")$byClass["Specificity"],
             confusionMatrix(data = rf_preds, reference = wdbc_test$y, positive = "M")$byClass["Specificity"])

# Created data frame with performance of models and add FN and FP rates
models_perf <- data.frame(Model = models, Accuracy = accuracy, Sensitivity = sensitivity, Specificity = specificity) %>%  mutate("False Negative Rate" = 1 - Sensitivity, "False Positive Rate" = 1 - Specificity)

models_perf
