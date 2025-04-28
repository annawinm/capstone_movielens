if (!require(tidyverse)) install.packages('tidyverse')
library(tidyverse)
if (!require(caret)) install.packages('caret')
library(caret)
if (!require(knitr)) install.packages('knitr')
library(knitr)
if (!require(lubridate)) install.packages('lubridate')
library(lubridate)


# Create edx and final_holdout_test sets from the MovieLens 10M dataset

# Note: this process could take a couple of minutes

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

options(timeout = 120)

dl <- "ml-10M100K.zip"
if(!file.exists(dl))
  download.file("https://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings_file <- "ml-10M100K/ratings.dat"
if(!file.exists(ratings_file))
  unzip(dl, ratings_file)

movies_file <- "ml-10M100K/movies.dat"
if(!file.exists(movies_file))
  unzip(dl, movies_file)

ratings <- as.data.frame(str_split(read_lines(ratings_file), fixed("::"), simplify = TRUE),
                         stringsAsFactors = FALSE)
colnames(ratings) <- c("userId", "movieId", "rating", "timestamp")
ratings <- ratings %>%
  mutate(userId = as.integer(userId),
         movieId = as.integer(movieId),
         rating = as.numeric(rating),
         timestamp = as.integer(timestamp))

movies <- as.data.frame(str_split(read_lines(movies_file), fixed("::"), simplify = TRUE),
                        stringsAsFactors = FALSE)
colnames(movies) <- c("movieId", "title", "genres")
movies <- movies %>%
  mutate(movieId = as.integer(movieId))

movielens <- left_join(ratings, movies, by = "movieId")

# Final hold-out test set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding") # if using R 3.6 or later
# set.seed(1) # if using R 3.5 or earlier

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in final hold-out test set are also in edx set
final_holdout_test <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from final hold-out test set back into edx set
removed <- anti_join(temp, final_holdout_test)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#------------------------------

## EDA

# the number of rows and columns in edx
dim(edx)

# head of edx
head(edx, n = 4) %>% kable(caption = "Edx dataset")

# The number of different movies 
unique_movies <- edx %>% distinct(movieId)  

# The number of different users
unique_users <- edx %>% distinct(userId) 

# The number of different genre combinations
unique_genres <- edx %>% distinct(genres) 

# creating a table of the unique movies, users and genre combo's
tibble(unique_var = c("movies", "users", "genre combo's"),
       count = c(nrow(unique_movies), nrow(unique_users), 
                 nrow(unique_genres))) %>% 
  kable(caption = "Unique data variables in edx")

# Data distributions

#  plot the distribution of number of ratings per movie 
edx %>% group_by(movieId) %>% # group by movieID
  summarise(count = n()) %>% # count grouped rows
  ggplot(aes(x = count)) + # x-axis is the count
  geom_histogram(color = "black") + # plot histogram
  ggtitle("Movies' rating distribution") + # plot title
  xlab("Rating count (log10)") + # x-axis label
  ylab("Number of movies") + # y-axis label
  scale_x_log10(n.breaks = 10) + # make x-axis log base 10, 10 axis marks
  scale_y_continuous(n.breaks = 10) + # 10 y-axis marks
  theme_minimal() + # set the theme
  theme(plot.title = element_text(size = 10), # set the text size of title 
        axis.title = element_text(size = 8), # set the text size of axis label
        axis.text = element_text(size = 8)) # set the text size of axis marks


#  plot the distribution of number of ratings per user
edx %>% group_by(userId) %>% # group_by userID
  summarize(count = n()) %>% # count grouped rows
  ggplot(aes(count)) + # x-axis is the count
  geom_histogram(colour = "black") + # plot histogram
  ggtitle("User's rating distribution") + # plot title
  xlab("Rating count (log10)") + # x-axis label
  ylab("Number of users") + # y-axis label
  scale_x_log10(n.breaks = 10) + # make x-axis log base 10, 10 axis marks
  scale_y_continuous(n.breaks = 10) + # 10 y-axis marks
  theme_minimal() + # set the theme
  theme(plot.title = element_text(size = 10), # set the text size of title 
        axis.title = element_text(size = 8), # set the text size of axis label
        axis.text = element_text(size = 8)) # set the text size of axis marks


# Individual genres
# detect how many of each genre in the dataset using the sapply() and str_detect() functions
# list of most common genres
genres = c("Action", "Adventure", "Animation", 
           "Children", "Comedy", "Crime", 
           "Documentary", "Drama", "Fantasy", 
           "Film-Noir", "Horror", "Musical", 
           "Mystery", "Romance", "Sci-Fi", 
           "Thriller", "War", "Western") 
# create tibble of genres and count, where genres = each individual genre and
# count = sapply function that detects and sums each genre string contained in the
# genres column of edx
genres_count <- tibble( 
  genres = genres, 
  count = sapply(genres, function(g) { 
    sum(str_detect(edx$genres, g)) 
  })
)

# plot the number of ratings per genre
genres_count %>% # data from individual genre count tibble
  ggplot(aes(x = reorder(genres, -count), y = count)) + # plot with order count least to most  
  geom_bar(stat = "identity", width = 0.5) + # bar graph with x and y axis 
  ggtitle("Number of ratings per genre") + # title
  xlab("Genres") + # x-axis label
  ylab("Rating count") + # y-axis label
  scale_y_continuous(n.breaks = 10) + # 10 y-axis marks
  coord_flip() + # flip the x and y axes
  theme_minimal() + # set the theme
  theme(plot.title = element_text(size = 10),  # set the text size of title 
        axis.title = element_text(size = 8), # set the text size of axis label
        axis.text = element_text(size = 8), # set the text size of axis marks
        axis.text.x=element_text(angle=45, hjust=0.9)) # adjust the angle of x-axis marks


# plot the number of ratings per year
edx %>% mutate(year = year(as_datetime(timestamp))) %>% # new column converting timestamp into year using lubridate::as_datestamp
  group_by(year) %>% # group by year
  summarise(count = n()) %>% # count grouped rows
  ggplot(aes(year, count)) + # plot year by count
  geom_col(width = 0.5) + # column graph with x and y axes
  ggtitle("Number of ratings per year") + # title
  xlab("Year") + # x-axis label
  ylab("Rating count") + # y-axis label
  scale_y_continuous(n.breaks = 10) + # 10 y axis marks
  scale_x_continuous(n.breaks = 14) + # 14 x axis marks
  coord_flip() +  # flip the x and y axes
  theme_minimal() + # set the theme
  theme(plot.title = element_text(size = 10),  # set the text size of title 
        axis.title = element_text(size = 8), # set the text size of axis label
        axis.text = element_text(size = 8), # set the text size of axis marks
        axis.text.x=element_text(angle=45, hjust=0.9)) # adjust the angle of x-axis marks

#--------------------------
# Machine learning strategy

# table of characteristics to build into parameters of the machine learning model
tibble(iteration = (1:7),
       method_charactersistics = c("Average baseline", "Movie effects", 
                                   "Movie + user effects", 
                                   "Movie + user + genre effects",
                                   "Regularised movie effects",
                                   "Regularised movie + user effects",
                                   "Regularised movie + user + genre effects")) %>%  
  kable(caption = "Characterstics to build the model")

# RMSE written out as an R function.
# RMSE is the sqrt of the mean of (true_ratings - predicted_ratings) squared 
RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

# The following code creates a train set and a test set from 
# the edx MovieLens dataset. 

#set the seed with rounding
set.seed(1, sample.kind="Rounding")

# create the partition index using caret::createDataPartion function
test_index <- createDataPartition(y = edx$rating, times = 1,
                                  p = 0.2, list = FALSE)

# use the partition index to create the train_set and test_set
train_set <- edx[-test_index,]
test_set <- edx[test_index,]

# Use semi-join() function so that the test set doesn't include users 
# and movies that are not also in the train set. 
test_set <- test_set %>% 
  semi_join(train_set, by = "movieId") %>%
  semi_join(train_set, by = "userId")

#-----------------------------

# Building linear model

# Iteration 1:

# determine mu_hat as the average of all ratings in the train_set
mu_hat <- mean(train_set$rating)


# determine RSME of the baseline average model using the caret::RSME function on  
# the test_set and with mu_hat as a parameter.
rmse_average <- RMSE(test_set$rating, mu_hat)
rmse_average

#-------

# Iteration 2 movie effects:

# plot distribution of average movie ratings using ggplot histogram
train_set %>% # using the train_set
  group_by(movieId) %>% # group by movieID
  summarize(b_i = mean(rating)) %>%  # define b_i as the mean of ratings per movie 
  ggplot(aes(b_i)) + # plot b_i on x axis
  geom_histogram(bins = 30, color = "black") + # use histogram
  ggtitle("Distribution of average rating per movie") +
  xlab("Average movie ratings (b_i)") +
  ylab("Count") +
  scale_y_continuous(n.breaks = 10) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 8))

# using least squares to estimate b_i_hat on the train_set
# to determine the average for each movie we use the group_by() function
movie_averages <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i_hat = mean(rating - mu_hat))

# plot distribution of the estimation of b_i using ggplot histogram
movie_averages %>% ggplot(aes(b_i_hat)) + # plot b_i_hat
  geom_histogram(colour = "black") + # as histogram
  ggtitle("Distribution of b_i_hat") +
  xlab("b_i_hat") +
  ylab("Count") +
  scale_y_continuous(n.breaks = 10) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 8))

# predicting the ratings using the test_set by joining the baseline model 
# using left_join() to the movie averages, and pulling b_i_hat for evaluation
pred_ratings_b_i <- mu_hat + test_set %>%
  left_join(movie_averages, by = "movieId") %>%
  pull(b_i_hat)

# evaluating the plus movie effects model predictions using caret::RMSE
# on the test_set rating
rmse_movie_effects <- RMSE(pred_ratings_b_i, test_set$rating)
rmse_movie_effects 

#---------

# Iteration 3 user effects

# plot distribution of average user ratings using ggplot histogram
train_set %>% 
  group_by(userId) %>% # group by userID
  summarize(b_u = mean(rating)) %>% # define b_u as the mean of ratings per user
  ggplot(aes(b_u)) + # plot b_u on x-axis
  geom_histogram(bins = 30, color = "black") + # as histogram
  ggtitle("Distribution of average rating per user") +
  xlab("Average user ratings (b_u)") +
  ylab("Count") +
  scale_y_continuous(n.breaks = 10) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 8))

# using least squares to estimate b_u_hat on the train_set
user_averages <- train_set %>% 
  # left_join with the movie_averages dataset to add in the b_i_hat field
  left_join(movie_averages, by = "movieId") %>%
  # to determine the average for each user we use the group_by() function
  group_by(userId) %>%
  # summarise b_u_hat as the mean of rating - mu_hat - b_i_hat
  summarise(b_u_hat = mean(rating - mu_hat - b_i_hat))

# plot distribution of the estimation of b_u using ggplot histogram
user_averages %>% ggplot(aes(b_u_hat)) + # plot b_u_hat
  geom_histogram(colour = "black") + # as histogram
  ggtitle("Distribution of b_u_hat") +
  xlab("b_u_hat") +
  ylab("Count") +
  scale_y_continuous(n.breaks = 10) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 8))

# predicting the ratings using the test_set, by joining the movie effects model 
# using left_join() to the movie averages, and to join to the user_averages
# predicted ratings is the average + movie_average + user_average
predicted_ratings_b_u_hat <- test_set %>%
  left_join(movie_averages, by  = "movieId") %>%
  left_join(user_averages, by = "userId") %>%
  mutate(predicted = mu_hat + b_i_hat + b_u_hat) %>%
  pull(predicted)

# evaluating the plus user effects model predictions using caret::RMSE
rmse_user_effects<- RMSE(predicted_ratings_b_u_hat, test_set$rating)
# print rmse_user_effects
rmse_user_effects

#------

# Iteration 4 genre effects

# using least squares to estimate b_g_hat on the train_set
genre_averages <- train_set %>% 
  # left_join with the movie_averages dataset to add in the b_i_hat field
  left_join(movie_averages, by ="movieId") %>%
  # left_join with the movie_averages dataset to add in the b_u_hat field
  left_join(user_averages, by = "userId") %>%
  # group by genres
  group_by(genres) %>%
  # summarise b_g_hat as the mean of rating - mu_hat - b_i_hat - b_u_hat
  summarise(b_g_hat = mean(rating - mu_hat - b_i_hat - b_u_hat))

# plot distribution of the estimation of b_g using ggplot histogram
genre_averages %>% ggplot(aes(b_g_hat)) + # plot b_g_hat
  geom_histogram(colour = "black") + # as histogram
  ggtitle("Distribution of b_g_hat") +
  xlab("b_g_hat") +
  ylab("Count") +
  scale_y_continuous(n.breaks = 10) +
  theme_minimal() +
  theme(plot.title = element_text(size = 10), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 8))

# predicting the ratings using the test_set
predicted_ratings_b_g_hat <- test_set %>%
  # using left_join() to the movie averages  
  left_join(movie_averages, by  = "movieId") %>%
  # using left_join() to the user averages
  left_join(user_averages, by = "userId") %>%
  # using left_join() to the genre averages
  left_join(genre_averages, by = "genres") %>%
  # predicted ratings is the estimated average + movie_average + user_average 
  #  + genre_average 
  mutate(predicted = mu_hat + b_i_hat + b_u_hat + b_g_hat) %>%
  pull(predicted)

# evaluating the plus genre effects model predictions using caret::RMSE
rmse_genre_effects <- RMSE(predicted_ratings_b_g_hat, test_set$rating)
# print rmse_genre_effects
rmse_genre_effects

#--------

# table of rmse improvements on the linear model

rmses <- tibble(iteration = 1:4,
                method = c("Average baseline", "Movie effects", 
                           "Movie + user effects", "Movie + user + genre effects"),
                RMSE = c(rmse_average, rmse_movie_effects, 
                         rmse_user_effects, rmse_genre_effects))

rmses %>% kable(caption = "RMSE improvements over four models")

#-------

# Iteration 5 regularised movie effects


# assign the unique movie_titles in the edx dataset
movie_titles <- edx %>% 
  # select movieID and title
  select(movieId, title) %>%
  # distinct
  distinct()

# Ten best movies in movie_averages
best_ten_i <- movie_averages %>% 
  # left_join movie_averages to movie_titles
  left_join(movie_titles, by="movieId") %>%
  # arrange the table by greatest b_i_hat 
  arrange(desc(b_i_hat)) %>% 
  # select title and b_i_hat
  select(title, b_i_hat) %>% 
  # select the top 10 rows
  slice(1:10) %>%  
  # pull the title
  pull(title)

# putting best 10 movies into a table   
tibble(best_ten = (1:10),
       movie_titles = best_ten_i) %>% 
  kable(caption = "Ten best movies from movie effects model")

# number of ratings for the ten best recommended movies
number_ratings <- train_set %>% 
  # count movieID ratings in train set
  count(movieId) %>% 
  # left join movie averages to get b_i_hat
  left_join(movie_averages) %>%
  # left join movie titles for distinct movies
  left_join(movie_titles, by="movieId") %>%
  # arrange the table by greatest b_i_hat 
  arrange(desc(b_i_hat)) %>% 
  # select the top 10 rows
  slice(1:10) %>% 
  # pull the number of rows
  pull(n)

# make a tibble of number of top ten movie ratings
tibble(best_ten = (1:10),
       number_of_movie_ratings = number_ratings) %>%
  kable(caption = "Number of ratings for best ten movies")

# cross validation to tune for which value of lambda to use to regularise the 
# movie effects model

# define lambdas
lambdas <- seq(0, 10, 0.1)

# this function uses sapply and returns the RMSE of the regularised 
# movie effects model for each value of lambda in lambdas
tune_lambda <- sapply(lambdas, function(lambda){
  
  # compute mu_hat as the mean of rating in the train_set
  mu_hat <- mean(train_set$rating)
  
  # compute b_i_hat with train_set, group_by movieID
  # calculation using each value of lambda in lambdas
  reg_movie_effects <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i_hat = sum(rating - mu_hat)/(n()+lambda))
  
  # predict ratings against the test set
  # using calculated b_i_hat and mu_hat
  predicted_ratings <- test_set %>%
    left_join(reg_movie_effects, by = "movieId") %>%
    mutate(prediction = mu_hat + b_i_hat) %>%
    pull(prediction)
  
  # find nd return RMSE using caret::RMSE with predicted ratings and the test_set rating
  return(RMSE(predicted_ratings, test_set$rating))
})

# plot the lambda against RMSE from tune_lambda function using ggplot
# create a tibble of lambdas and RMSEs
tibble(lambdas = lambdas, RMSEs = tune_lambda) %>%
  ggplot(aes(lambdas, RMSEs)) + 
  geom_line() + # line graph
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 10) +
  ggtitle("Tuning lambda - movie effects") +
  xlab("Lambda") +
  ylab("RMSEs") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 8))  

# the best lambda is the one that minimises RMSE from function tune_lambda 
best_lambda <- lambdas[which.min(tune_lambda)]
# print the best lambda
best_lambda

# the RMSE of regularised movie effects is the minimal RMSE from tune_lambda
regularised_rmse_movie_effects <- min(tune_lambda)
# print the RMSE
regularised_rmse_movie_effects

# using the best lambda 
reg_just_movie_effects <- train_set %>%
  group_by(movieId) %>%
  summarise(b_i_hat = sum(rating - mu_hat)/(n()+best_lambda)) 

# best 10 movies with regularisation of movie effects
best_ten_movies <- reg_just_movie_effects %>% 
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i_hat)) %>% 
  select(title) %>% 
  head(10) %>%
  pull(title)

# putting into a table   
tibble(best_ten = (1:10),
       movie_titles = best_ten_movies) %>% 
  kable(caption = "Ten best movies with regularised movie effects")

# number of ratings for the ten best recommended movies
number_ratings_reg <- train_set %>% 
  # count movieID ratings in train set
  count(movieId) %>% 
  # left join reg_just_movie_effects to get b_i_hat_(lambda)
  left_join(reg_just_movie_effects) %>%
  # left join movie titles for distinct movies
  left_join(movie_titles, by="movieId") %>%
  # arrange the table by greatest b_i_hat 
  arrange(desc(b_i_hat)) %>% 
  # select the top 10 rows
  slice(1:10) %>% 
  # pull the number of rows
  pull(n)

# make a tibble of number of top ten movie ratings
tibble(best_ten = (1:10),
       number_of_movie_ratings = number_ratings_reg) %>%
  kable(caption = "Number of ratings for top ten movies using regularisation")

#---------

# Iteration 6 regularised user effects

# cross validation to tune for which value of lambda to use to regularise the 
# movie + user effects model

# define lambdas
lambdas <- seq(0, 10, 0.1)

# this function uses sapply and returns the RMSE of the regularised 
# movie + user effects model for each value of lambda in lambdas
tune_lambda_u <- sapply(lambdas, function(lambda){
  
  # compute mu_hat as the mean of rating in the train_set
  mu_hat <- mean(train_set$rating)
  
  # compute b_i_hat with train_set, group_by movieID
  # calculation using each value of lambda in lambdas  
  reg_movie_effects <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i_hat = sum(rating - mu_hat)/(n()+lambda))
  
  # compute b_u_hat with train_set, group_by userID
  # left join with b_i_hat
  # calculation using each value of lambda in lambdas
  reg_user_effects <- train_set %>%
    group_by(userId) %>%
    left_join(reg_movie_effects, by = "movieId") %>%
    summarize(b_u_hat = sum(rating - mu_hat - b_i_hat)/(n()+lambda))
  
  # predict ratings against the test set
  # using calculated b_u_hat, b_i_hat and mu_hat    
  predicted_ratings <- test_set %>%
    left_join(reg_movie_effects, by = "movieId") %>%
    left_join(reg_user_effects, by = "userId") %>%
    mutate(prediction = mu_hat + b_i_hat + b_u_hat) %>%
    pull(prediction)
  
  # find and return RMSE using caret::RMSE with predicted ratings and the test_set rating    
  return(RMSE(predicted_ratings, test_set$rating))
})

# plot the lambda against RMSE from tune_lambda_u function using ggplot
# create a tibble of lambdas and RMSEs
tibble(lambdas = lambdas, RMSEs = tune_lambda_u) %>%
  ggplot(aes(lambdas, RMSEs)) + # plot lambdas against RMSEs
  geom_line() + # line graph
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 10) +
  ggtitle("Tuning lambda - movie + user effects") +
  xlab("Lambda") +
  ylab("RMSEs") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 8)) 

# the best lambda is the one that minimises RMSE in the function tune_lambda_u
best_lambda <- lambdas[which.min(tune_lambda_u)]
# print best lambda
best_lambda

# the RMSE of regularised movie + user effects is the minimal RMSE from tune_lambda_u
regularised_rmse_i_u_effects <- min(tune_lambda_u)
# print RMSE
regularised_rmse_i_u_effects 

#---------

# Iteration 7 regularised genre effects

# cross validation to tune for which value of lambda to use to regularise the 
# movie + user + genre effects model

# define lambdas
lambdas <- seq(0, 10, 0.1)

# this function uses sapply and returns the RMSE of the regularised 
# movie + user effects model for each value of lambda in lambdas
tune_lambda_all <- sapply(lambdas, function(lambda){
  
  # compute mu_hat as the mean of rating in the train_set
  mu_hat <- mean(train_set$rating)
  
  # compute b_i_hat with train_set, group_by movieID
  # calculation using each value of lambda in lambdas  
  reg_movie_effects <- train_set %>%
    group_by(movieId) %>%
    summarize(b_i_hat = sum(rating - mu_hat)/(n()+lambda))
  
  # compute b_u_hat with train_set, group_by userID
  # left join with b_i_hat
  # calculation using each value of lambda in lambdas
  reg_user_effects <- train_set %>%
    group_by(userId) %>%
    left_join(reg_movie_effects, by = "movieId") %>%
    summarize(b_u_hat = sum(rating - mu_hat - b_i_hat)/(n()+lambda))
  
  # compute b_g_hat with train_set, group_by genres
  # left join with b_i_hat and b_u_hat
  # calculation using each value of lambda in lambdas
  reg_genre_effects <- train_set %>%
    group_by(genres) %>%
    left_join(reg_movie_effects, by = "movieId") %>%
    left_join(reg_user_effects, by = "userId") %>%
    summarise(b_g_hat = sum(rating - mu_hat - b_i_hat - b_u_hat)/(n()+lambda))
  
  # predict ratings against the test set
  # using calculated b_g_hat, b_u_hat, b_i_hat and mu_hat    
  predicted_ratings <- test_set %>%
    left_join(reg_movie_effects, by = "movieId") %>%
    left_join(reg_user_effects, by = "userId") %>%
    left_join(reg_genre_effects, by = "genres") %>%
    mutate(prediction = mu_hat + b_i_hat + b_u_hat + b_g_hat) %>%
    pull(prediction)
  
  # find and return RMSE using caret::RMSE with predicted ratings and the test_set rating        
  return(RMSE(predicted_ratings, test_set$rating))
})

# plot the lambda against RMSE from tune_lambda_all function using ggplot
# create a tibble of lambdas and RMSEs
tibble(lambdas = lambdas, RMSEs = tune_lambda_all) %>%
  ggplot(aes(lambdas, RMSEs)) +
  geom_line() + # line graph
  scale_y_continuous(n.breaks = 10) +
  scale_x_continuous(n.breaks = 10) +
  ggtitle("Tuning lambda - movie + user + genre effects") +
  xlab("Lambda") +
  ylab("RMSEs") +
  theme_minimal() +
  theme(plot.title = element_text(size = 10), 
        axis.title = element_text(size = 8), 
        axis.text = element_text(size = 8)) 

# the best lambda is the one that minimises RMSE in the function tune_lambda_all
best_lambda_all <- lambdas[which.min(tune_lambda_all)]
# print best lambda
best_lambda_all

# the RMSE of regularised movie + user + genre effects is the minimal RMSE 
# from tune_lambda_all
regularised_rmse_all_effects <- min(tune_lambda_all)
#print RMSE
regularised_rmse_all_effects

#-------------------------------------

# Final model and evalutation

# Table of the RMSEs for all model iterations
rmses_all <- tibble(method = c("Average baseline", "Movie effects", 
                               "Movie + user effects", 
                               "Movie + user + genre effects", 
                               "Regularised movie effects", 
                               "Regularised movie + user effects",  
                               "Regularised movie + user + genre effects"),
                    RMSE = c(rmse_average, rmse_movie_effects, 
                             rmse_user_effects, rmse_genre_effects, 
                             regularised_rmse_movie_effects, 
                             regularised_rmse_i_u_effects, regularised_rmse_all_effects))

rmses_all %>% kable(caption = "RMSE improvements over seven models")

# use final regularised algorithm on edx dataset to create the final model
# evaluate the accuracy of final model on the final_holdout_test

# compute mu_hat as the mean of rating in edx
mu <- mean(edx$rating)

# compute b_i with edx, 
# group_by movieID
# calculation using tuned lambda from final algorithm  
b_i <- edx %>%
  group_by(movieId) %>%
  summarise(b_i = sum(rating - mu)/(n()+best_lambda_all))

# compute b_u with edx, 
# left join with b_i
# group_by userID
# calculation using tuned lambda from final algorithm 
b_u <- edx %>%
  left_join(b_i, by = "movieId") %>%
  group_by(userId) %>%
  summarise(b_u = sum(rating - mu - b_i)/(n()+best_lambda_all))

# compute b_g with edx, 
# left join with b_i
# left join with b_u
# group_by genres
# calculation using tuned lambda from final algorithm 
b_g <- edx %>% 
  left_join(b_i, by ="movieId") %>%
  left_join(b_u, by = "userId") %>%
  group_by(genres) %>%
  summarise(b_g = sum(rating - mu - b_i - b_u)/(n()+best_lambda_all))

# predict final model ratings against the final_holdout_test
# using calculated b_g, b_u, b_i and mu    
final_model_ratings <- final_holdout_test %>%
  left_join(b_i, by ="movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(predictions = mu + b_i + b_u + b_g) %>%
  pull(predictions)

# calculate the final model RMSE using caret::RMSE with final model ratings 
# and the final holdout test ratings    
rmse_final_model <- RMSE(final_model_ratings, final_holdout_test$rating) 

# table of the final model RMSE evaluated against final_holdout_test
tibble(final_method = "Regularised movie + user + genre effects",
       RMSE = rmse_final_model) %>%
  kable(caption = "RMSE for the final model")

#-----------------------------------------

# Movie recommendations

# final optimised model used to make predictions with edx dataset
# using, mu, b_i, b_u and b_g already calculated on edx
optimised_model <- edx %>%
  left_join(b_i, by ="movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_g, by = "genres") %>%
  mutate(predictions = mu + b_i + b_u + b_g)

# best 10 movies using final optimised model
best_ten_movies_reg <- optimised_model %>%
  arrange(desc(predictions)) %>%
  group_by(title) %>%
  distinct(title) %>%
  head(10) %>%
  pull(title)

# table of 10 best overall movies using optimised model
tibble(best_ten = (1:10),
       movie_titles = best_ten_movies_reg) %>%
  kable(caption = "Ten best rated movies using optimised model")

# 10 best science fiction movies using final optimised model
best_ten_scifi <- optimised_model %>%
  filter(str_detect(genres, "Sci-Fi")) %>%
  arrange(desc(predictions)) %>%
  group_by(title) %>%
  distinct(title) %>%
  head(10) %>%
  pull(title)

# table of 10 best science fiction movies using optimised model
tibble(best_ten = (1:10),
       science_fiction = best_ten_scifi) %>%
  kable(caption = "Ten best rated science fiction movies")

# 10 best drama movies using final optimised model
best_ten_drama <- optimised_model %>%
  filter(str_detect(genres, "Drama")) %>%
  arrange(desc(predictions)) %>%
  group_by(title) %>%
  distinct(title) %>%
  head(10) %>%
  pull(title)

# table of 10 best drama movies using optimised model
tibble(best_ten = (1:10),
       drama = best_ten_drama) %>%
  kable(caption = "Ten best rated drama movies")

#--------- end ----------------


