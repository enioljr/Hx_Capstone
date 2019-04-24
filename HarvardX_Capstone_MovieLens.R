#############################################################
# Create edx set, validation set, and submission file
#############################################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- read.table(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                      col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1)
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set

validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set

removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

# Cleaning your data, exploring and visualizing to find patterns or correlations

head(edx)

# loading some libraries to work with data found
library(ggplot2)
library(dplyr)
library(lubridate)
library(tidyr)

glimpse(edx)
summary(edx)
n_distinct(edx$movieId)
n_distinct(edx$genres)
n_distinct(edx$userId)

# we note that "timestamp" should be converted to a more friendly format

edx <- mutate(edx, year_tstamp = year(as_datetime(timestamp)))
head(edx)
# The column "year_tstamp" means the year the movie was first rated.

# we still have to split the movie debut date in the "title" column in order to have more
# data to compare and explore

edx_debut_year <- edx %>%
  # trim whitespaces
  mutate(title = str_trim(title)) %>%
  # split title to title_tmp, debut_year
  extract(title, c("title_tmp", "debut_year"), regex = "^(.*) \\(([0-9 \\-]*)\\)$", remove = FALSE) %>%
  # drop title_tmp column and 
  select(-title_tmp)
summary(edx_debut_year)
edx_debut_year$debut_year <- as.numeric(edx_debut_year$debut_year)
class(edx_debut_year$debut_year)
edx_debut_year <- edx_debut_year %>% select(-timestamp)
summary(edx_debut_year)

#The individual movies' age
edx_debut_year <- edx_debut_year %>% mutate(movie_age = 2019 - debut_year)
head(edx_debut_year)
# reordering the columns
edx_clean <- edx_debut_year[, c(1, 2, 3, 4, 6, 5, 7, 8)]
summary(edx_clean) # check the data for NA's or any irregularity

# Now a little bit of data exploratory analisys
# Let's analyse every single feature and combine some of the fetaures to get something from the data:

# The userId and movieId has been discussed and will not be evaluated now.
# The 'rating' feature for movies and users:
edx_clean %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line(col = "blue") # checking the frequency of the ratings

# The users ratings:
user_avg_rat <- edx_clean %>% group_by(userId) %>% summarize(user_avg_rat = mean(rating))
ggplot(user_avg_rat, aes(user_avg_rat, userId)) +
  coord_flip() +
  geom_point(alpha = 0.3, colour = "red") +
  ggtitle("Users vs. User's rating")
summary(user_avg_rat)
# Here it shows an even distribution between 3.0 and 4.5

# Most rated movies:
most_rated <- edx_clean %>%
  group_by(title) %>% summarise(rating_n =n()) %>% arrange(desc(rating_n))
head(most_rated,20)

most_rated_avg <- edx_clean %>%
  group_by(title) %>% summarise(rating_avg = mean(rating)) %>% arrange(desc(rating_avg))
head(most_rated_avg, 20)
# there is a clear difference between the number of ratings a movie has received compared to the
# rating average.
most_rated_avg_n <- left_join(most_rated, most_rated_avg, by = "title")
# On the next plot we notice that most of the movies were rated less than 5,000 times with
# some movies being rated above rating 4.0 with very few evaluations.
ggplot(most_rated_avg_n, aes(rating_n, rating_avg)) + geom_point(alpha = 0.3, col = "green")
head(most_rated_avg_n, 20)

# Is there a correlation?
cor_n_avg_rat <- cor(most_rated$rating_n, most_rated_avg$rating_avg)
# according to Deborah J. Rumsey (Statistics, 3rd Edition), we have
# a value of +0.50. A moderate uphill (positive) relationship should be considered.
ggplot(most_rated_avg_n, aes(rating_n, rating_avg)) + stat_bin_2d() +
  scale_fill_gradientn(colours = c("green", "yellow", "blue", "red")) +
  stat_smooth(method = "lm", color = "blue", size = 0.5) +
  annotate("text", x = 22000, y = 5.0, label = cor_n_avg_rat,
           parse = TRUE, color = "grey50", size = 5) + ylab("Average Movie Rating") + xlab("Number of Ratings")

summary(lm(rating_avg ~ rating_n, data = most_rated_avg_n))

# age of the movie
movie_age <- edx_clean %>% group_by(movie_age) %>% summarize(count = n())
movie_age %>% ggplot(aes(movie_age, count)) + geom_point(colour = "mediumspringgreen") + geom_line()
summary(lm(count ~ movie_age, data = movie_age))

# there seems to be a split for movies younger than 25 yo and older, let's check:
movie_age_split_less25 <- edx_clean %>% filter(movie_age <= 25) %>%
  group_by(movie_age) %>% summarize(count = n())
summary(lm(count ~ movie_age, data = movie_age_split_less25)) # Now we have something if we consider movies younger than 25yo
# with a R-squared value of 0.93

# The same applies here with movies older than 25yo
movie_age_split_more25 <- edx_clean %>% filter(movie_age > 25) %>%
  group_by(movie_age) %>% summarize(count = n())
summary(lm(count ~ movie_age, data = movie_age_split_more25))
# This time the R-squared value is less than the previous case.

# For the Movies let's summarize with:
movie_avg_rat <- edx_clean %>% group_by(movieId) %>% summarize(movie_avg_rat = mean(rating))
movie_avg_yr_rat <- edx_clean %>% group_by(year_tstamp) %>% summarize(movie_avg_yr_rat = mean(rating))
movie_avg_age_rat <- edx_clean %>% group_by(movie_age) %>% summarize(movie_avg_age_rat = mean(rating))

# Let's explore a little bit more

movie_avg_age_rat %>%
  ggplot(aes(movie_age, movie_avg_age_rat)) +
  geom_point(colour = "Green") +
  ggtitle("Movie age vs. Rating by age")
summary(lm(movie_avg_age_rat ~ movie_age, data = movie_avg_age_rat)) # small R-square (0.34)

movie_avg_age_rat$movie_age[which.max(movie_avg_age_rat$movie_avg_age_rat)]
# we notice that the graph is almost linear between the maximum of 73yo and 25yo
movie_age_25_73 <- movie_avg_age_rat %>% filter((movie_age >= 25) & (movie_age <= 73))
summary(lm(movie_avg_age_rat ~ movie_age, data = movie_age_25_73))
# it makes difference in the R-Squared value now at 0.67
# There is a second value around 62yo, let's check it:
movie_age_25_62 <- movie_avg_age_rat %>% filter((movie_age >= 25) & (movie_age <= 62))
summary(lm(movie_avg_age_rat ~ movie_age, data = movie_age_25_62))
# little bit more improvement at 0.69 now.

# R = average for the movie (mean) = (Rating)
# v = number of votes for the movie = (votes)
# m = minimum votes required to be listed in the Top 250
# C = the mean vote across the whole report
weighted_rat <- function(R, v, m, C) {
  return (v/(v+m))*R + (m/(v+m))*C
}

movie_avg_rat_wt <- edx_clean %>%
  na.omit() %>%
  select(title, rating, debut_year) %>%
  group_by(title, debut_year) %>%
  summarise(count = n(), mean = mean(rating), min = min(rating), max = max(rating)) %>%
  ungroup() %>%
  arrange(desc(mean))

print(movie_avg_rat_wt)

movie_weighted_rat <- movie_avg_rat_wt %>%
  mutate(wr = weighted_rat(mean, count, 500, mean(mean))) %>%
  arrange(desc(wr)) %>%
  select(title, debut_year, count, mean, wr)

print(movie_weighted_rat)

ggplot(movie_weighted_rat, aes(x = debut_year, y = wr)) +
  geom_point(col = "blue")

# Now about the genres:
edx_genres <- edx_clean %>% separate_rows(genres, sep = "\\|")
genres_avg_rat <- edx_genres %>% group_by(genres) %>% summarize(genres_avg_rat = mean(rating))
ggplot(genres_avg_rat, aes(reorder(genres, genres_avg_rat), genres_avg_rat, fill= genres_avg_rat)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_distiller(palette = "Set2") +
  labs(y = "Rating", x = "Genre") +
  ggtitle("Distribution of Genres by Rating")
# here it does not seem that the rating change that much among the genres
genres_count <- edx_clean %>%
  separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarise(count = n()) %>%
  arrange(desc(count))
ggplot(genres_count, aes(reorder(genres, count), count, fill= count)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  scale_fill_distiller(palette = "Set1") +
  labs(y = "Count", x = "Genre") +
  ggtitle("Distribution of Genres by Rating Frequency")
# The three most rated genres are Drama, Comedy, and Action.

# The previous graphical representations suggest that we should give attention to
# the number of ratings, the value of the rating and to the average rating vs movie age.

# Modelling Movie Effects

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

mu_movie_rat <- mean(edx_clean$rating)
mu_movie_rat

movie_avgs <- edx_clean %>% 
  group_by(movieId) %>% 
  summarize(b_i = mean(rating - mu_movie_rat))
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("pink"))

## Modeling movie effects (b_i)

predicted_ratings <- mu_movie_rat + edx_clean %>% 
  left_join(movie_avgs, by='movieId') %>%
  pull(b_i)

model_1_rmse <- RMSE(predicted_ratings, edx_clean$rating)
model_1_rmse

## User effects (b_u)

edx_clean %>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")
# as we have seem on the previous plots, there is a consitency in the ratings between 2.5 and 4.5
# and up to more than 3000 ratings for some users. We could limit the b_u between these values
# or filter the number of users that rated more than a given threshold value of movies.

user_avgs <- edx_clean %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_movie_rat - b_i))
# constructing the predictors
predicted_ratings <- edx_clean %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_movie_rat + b_i + b_u) %>%
  pull(pred)

model_2_rmse <- RMSE(predicted_ratings, edx_clean$rating)
model_2_rmse

edx_clean %>% 
  left_join(movie_avgs, by='movieId') %>%
  mutate(residual = rating - (mu_movie_rat + b_i)) %>%
  arrange(desc(abs(residual))) %>% 
  select(title,  residual) %>% slice(1:10)
movie_titles <- edx_clean %>% 
  select(movieId, title) %>%
  distinct()

movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i) %>% 
  slice(1:10)
movie_avgs %>% left_join(movie_titles, by="movieId") %>%
  arrange(b_i) %>% 
  select(title, b_i) %>% 
  slice(1:10)

edx_clean %>% count(movieId) %>% 
  left_join(movie_avgs) %>%
  left_join(movie_titles, by="movieId") %>%
  arrange(desc(b_i)) %>% 
  select(title, b_i, n) %>% 
  slice(1:10)

# Using the Penalized Least Squares instead of limiting the observations (i.e: creating a
# subset of the train set with only user that rated more than 30 movies and less than 100)

#Choosing the Lamba value

lambdas <- seq(0, 1, 0.05)
mu <- mean(edx_clean$rating)
rmses <- sapply(lambdas, function(l){
  
  b_i <- edx_clean %>% 
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  b_u <- edx_clean %>% 
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
  predicted_ratings <- 
    edx_clean %>% 
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>% .$pred
  
  return(RMSE(predicted_ratings, edx_clean$rating))
})
qplot(lambdas, rmses)
lambdas[which.min(rmses)]


## Using the model on the Validation data
mu <- mean(validation$rating)
l <- 0.5
b_i <- validation %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n() + l))

b_u <- validation %>%
  left_join(b_i, by='movieId') %>% 
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n() +l))

predicted_ratings <- validation %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i +  b_u) %>% .$pred

RMSE(predicted_ratings, validation$rating)