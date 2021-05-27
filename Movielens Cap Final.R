## Author: Mazen Hamdoun



#Load necessary Libraries
library(tidyverse)
library(caret)
library(data.table)
library(parallel)

setwd("~/Documents/R/EDX Harvard Cap/Movielens")
edx <- read_csv2("movie.csv",col_names = T, col_types = cols("i","n","n","i","c","c"))
validation <- read_csv2("movie_val.csv",col_names = T, col_types = cols("i","n","n","i","c","c"))


#wrangle training data to split genres column into multiple rows (tidy form) and store for later reuse
edx_tidy <- edx %>% separate(genres, as.character(c('1':'8')), sep = "\\|") %>% gather(genres_t,genres,as.character("1":"8"),  na.rm = T)  
edx_tidy <- edx_tidy[,-6]

#Validation set wrangling to tidy format 
validation_tidy <- validation %>% separate(genres, as.character(c('1':'8')), sep = "\\|") %>% gather(genres_t,genres,as.character("1":"8"),  na.rm = T)  
validation_tidy <- validation_tidy[,-6]

### Rating Counts
#The histogram shows the distribution of ratings by groups (within bins) of movieID in log base 10 scale

#Count by Movie ID
edx %>% group_by(movieId) %>%
  summarize(n=n()) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Movies")

#Count by User ID
edx %>% group_by(userId) %>%
  summarize(n=n()) %>% 
  ggplot(aes(n)) + 
  geom_histogram(bins = 30, color = "black") + 
  scale_x_log10() + 
  ggtitle("Users")

### Movie Effect

edx %>% group_by(movieId) %>% summarise(Average=mean(rating)) %>% ggplot(aes(Average)) + geom_histogram( color = "black",bins = 20) + 
  ggtitle("Average Movie Rating")
 

### User Effect

edx %>% group_by(userId) %>% summarise(Average=mean(rating)) %>% ggplot(aes(Average)) + geom_histogram( color = "black",bins = 20) + 
  ggtitle("Average User Rating")


### Genres per user Effect Sample

uid <- edx %>% distinct(userId) 

#Sampling 10,000 users randomly for visualization
set.seed(1,sample.kind = "Rounding")
ug_rating <- edx_tidy[which(edx_tidy$userId %in% sample(uid$userId,10000,FALSE,NULL)),]  %>% group_by(userId,genres) %>%
               summarise(Average=mean(rating),n=n())  

ug_rating <- ug_rating %>% group_by(userId) %>% summarize(sum_wt=sum(n)) %>% 
              inner_join(ug_rating,by="userId") %>% 
              mutate(wtd_genres=(ug_rating$Average*ug_rating$n/sum_wt*100))

ug_rating %>% ggplot(aes(as.character(userId),wtd_genres,fill=genres)) + geom_bar(stat = "identity",position=position_fill()) #+ scale_x_log10()
#+ scale_x_log10()

# Build Genres per User LookUp Table (genres_LUT)

# Obtain list of distinct genres
genres_LUT<- edx %>% select(userId,genres) %>%  distinct()
# Separate genres column into separate columns 
genres_LUT <- genres_LUT %>% separate( genres ,paste("g",as.character(c('1':'8')),sep=""), sep = "\\|")
# Create mapping for each combination of genres into its component genres 
genres_LUT <-  genres_LUT %>% unite("t",g1:g8,sep = "|", na.rm=T, remove = F ) %>% gather(genres_t,genres,g1:g8,  na.rm = T)
#Drop the genres column headings [g1 to g8] which are unnecessary
genres_LUT <- genres_LUT[,-3] 


#The training data set (edx) is split into training and test sets to assess model performance

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

set.seed(1,sample.kind = "Rounding")
test_index <- createDataPartition(y = edx$rating, times = 1, p = 0.2,
                                  list = FALSE)
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

#ensure test and train set movie/user ID's are consistent
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId") %>%
  semi_join(train_set, by = c("userId","genres"))


# Regularization is used to reduce the undesirable effects of lack of data (users/movies/genres with very few ratings) on the model's predictive ability.

# lambda: regularization parameter that requires optimization
lambda_step <- 0.25
lambda_max <- 7
lambda_min <- 1

# range of lambdas that will be used for optimization
lambdas <- seq(lambda_min, lambda_max, lambda_step)


# The regularization parameter Lambda is optimized by splitting training data (train_set) into 2 subsets: 
#   - optimization training set (train_BS)
#   - optimization testing set (test_BS)
 
set.seed(1,sample.kind = "Rounding")
BS <- createDataPartition(y = train_set$rating, times = 1, p = 0.2,
                          list = FALSE)

train_BS <- train_set[-BS,]
test_BS <- train_set[BS,]

test_BS <- test_BS %>% 
  semi_join(train_BS, by = "movieId") %>%
  semi_join(train_BS, by = "userId") %>%
  semi_join(train_BS, by = c("userId","genres"))

##### lambda test value --- MUST BE REMOVED------------


#The following plot determines optimal Lambda (having the minimum RMSE) to be used in the model:
lambda <- function(l){

  #obtain expected value (mean) as base prediction
  mu <- mean(train_BS$rating)
  #regularize movie effect after removing average rating
  b_i <- train_BS %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  #regularize user effect after removing average rating and movie effect
  b_u <- train_BS %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  #obtain genres per user effect
  
  #calculate effect for each genre/user combination  
  train_bg <- left_join(train_BS, edx_tidy, by=c("movieId","userId")) %>% 
    select(userId, movieId, timestamp=timestamp.y, title=title.y, genres=genres.y, rating=rating.y)
  
  
  b_u_g_k <- train_bg %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(userId, genres) %>%
    summarize(b_g_k = sum(rating - b_u - b_i - mu)/(n()+l))
  
  #calculate effect for the combined genres per user  
  b_u_g <- inner_join(b_u_g_k, genres_LUT, by=c("userId","genres")) %>% group_by(userId,genres=t) %>% summarise(b_g=mean(b_g_k))

  # test_BS[which(is.na(predicted_ratings))] %>% left_join(b_u_g,by=c("userId","genres"))
  # lefty <- left_join(genres_LUT,b_u_g_k,by=c("userId","genres"))
  
  #predict rating
  predicted_ratings <-
    test_BS %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_u_g, by = c("userId","genres")) %>%
    mutate(pred = mu + b_i + b_u + b_g) %>%
    pull(pred)
  #return RMSE on the optimization test set for given lambda
  return(RMSE(predicted_ratings, test_BS$rating))
}

# apply the range of lambdas to determine optimal lambda
# NB mclapply allows parallel processing to speed computation time, hence is used instead of sapply
res <- mclapply(lambdas,lambda)
#flatten list of lists returned by mclapply to a numeric vector
res <- flatten_dbl(res)

#Plot RMSE obtained for different lambda values to visualize minimum (the optimal Lambda)
qplot(lambdas, res)

# optimal lambda has the min RMSE
opt_lambda <- lambdas[which.min(res)]


#Now, we train the model with our optimal lambda on the training set (train_set) and test model output on our test set (test_set).

#obtain expected value as main prediction
mu <- mean(train_set$rating)
#regularize movie effect after removing average rating
b_i <- train_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+opt_lambda))

predicted_ratings <-
  test_set %>%
  left_join(b_i, by = "movieId")  %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)
pred_ratings <- RMSE(predicted_ratings, test_set$rating)

pred_ratings

#regularized user effect after removing average rating and movie effect
b_u <- train_set %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+opt_lambda))

predicted_ratings <-
  test_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u ) %>%
  pull(pred)
pred_ratings <- RMSE(predicted_ratings, test_set$rating)

pred_ratings

# Model performance improves further after adding the genres per user effect : 

#obtain genres effect

#calculate effect for each genre/user combination  
train_bg <- left_join(train_set, edx_tidy, by=c("movieId","userId")) %>% 
  select(userId, movieId, timestamp=timestamp.y, title=title.y, genres=genres.y, rating=rating.y)

b_u_g_k <- train_bg %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(userId, genres) %>%
  summarize(b_g_k = sum(rating - b_u - b_i - mu)/(n()+opt_lambda))

#calculate effect for the combined genres per user  
b_u_g <- inner_join(b_u_g_k, genres_LUT, by=c("userId","genres")) %>% group_by(userId,genres=t) %>% summarise(b_g=mean(b_g_k))

#predict rating
predicted_ratings <-
  test_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_u_g, by = c("userId","genres")) %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
#return RMSE on test set for optimal lambda
pred_ratings <- RMSE(predicted_ratings, test_set$rating)

pred_ratings

#We now train the final model including all effects on the full training data set (edx):

mu <- mean(edx$rating)
#regularize movie effect after removing average rating
b_i <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+opt_lambda))
#regularized user effect after removing average rating and movie effect
b_u <- edx %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+opt_lambda))
#genres
#calculate effect for each genre/user combination  
train_bg <- left_join(edx, edx_tidy, by=c("movieId","userId")) %>% 
  select(userId, movieId, timestamp=timestamp.y, title=title.y, genres=genres.y, rating=rating.y)

b_u_g_k <- train_bg %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(userId,genres) %>%
  summarize(b_g_k = sum(rating - b_u - b_i - mu)/(n()+opt_lambda))

#calculate effect for the combined genres per user  
b_u_g <- inner_join(b_u_g_k, genres_LUT, by=c("userId","genres")) %>% group_by(userId,genres=t) %>% summarise(b_g=mean(b_g_k))


## Results on validation (hold-out) dataset

# Predict rating on validation set to obtain final RMSE
# Since not all userId/genres combinations existing in the validation are in the training set, we consider these add nothing (zero) to our prediction, 
# as can be seen from the ifelse(is.na(b_g),0,b_g) portion of the code

predicted_ratings <-
  validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_u_g, by = c("userId","genres")) %>%
  mutate(pred = mu + b_i + b_u + ifelse(is.na(b_g),0,b_g)) %>%
  pull(pred)
#return RMSE on validation set
final_RMSE <- RMSE(predicted_ratings, validation$rating)

#range(predicted_ratings[which(predicted_ratings-5>0)])

final_RMSE

# By keeping only userId/genres combination present in both validation and training (edx) sets, 
# results obviously improve further
validation_set <- validation %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId") %>%
  semi_join(edx, by = c("userId","genres"))

predicted_ratings <-
  validation_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_u_g, by = c("userId","genres")) %>%
  mutate(pred = mu + b_i + b_u + b_g) %>%
  pull(pred)
#return RMSE on validation set
final_RMSE <- RMSE(predicted_ratings, validation_set$rating)

rm(edx_tidy,edx,validation,validation_set,validation_tidy)