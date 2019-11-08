#loading the data used to generate script

load(file="edx.RData")
load(file="validation.RData")

#installing packages used to generate script

if(!require(dplyr))
  install.packages("dplyr", repos = "http://cran.us.r-project.org")
library(dplyr)

if(!require(stringr))
  install.packages("stringr", repos = "http://cran.us.r-project.org")
library(stringr)

if(!require(caret))
  install.packages("caret", repos = "http://cran.us.r-project.org")
library(caret)

#Create a "Release Year" column in the validation and edx sets

years<-str_sub(validation$title,start=-6) %>% str_replace_all(.,"\\(|\\)","")
validation <- validation %>% mutate(release_year=as.numeric(years))

years<-str_sub(edx$title,start=-6) %>% str_replace_all(.,"\\(|\\)","")
edx <- edx %>% mutate(release_year=as.numeric(years))

#Use our optimized lambda from the test sets and run the code using the edx and validation sets.

lambda <- 4.25

#Base model is the overall mean rating for the data

mu <- mean(edx$rating)

#Adjusting the base for differences in movie averages

movie_reg_avgs <- edx %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating-mu)/(n() + lambda))

#Adjusting the base for differences in user averages

user_reg_avgs <- edx %>%
  left_join(movie_reg_avgs, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating-b_i-mu)/(n() + lambda))

#Adjusting the base for differences in genres

genre_reg_avgs <- edx %>%
  left_join(movie_reg_avgs, by="movieId") %>%
  left_join(user_reg_avgs, by = "userId") %>%
  group_by(genres) %>%
  summarize(b_g = sum(rating-b_i-b_u-mu)/(n() + lambda))

#Adjusting the base for differences in years

year_reg_avgs <- edx %>%
  left_join(movie_reg_avgs, by= "movieId") %>%
  left_join(user_reg_avgs, by = "userId") %>%
  left_join(genre_reg_avgs, by = "genres") %>%
  group_by(release_year) %>%
  summarize(b_y = sum(rating-b_i-b_u-b_g-mu)/(n() + lambda))

#Predicting ratings for the validation set

predicted_ratings <- validation %>%
  left_join(movie_reg_avgs, by= "movieId") %>%
  left_join(user_reg_avgs, by = "userId") %>%
  left_join(genre_reg_avgs, by = "genres") %>%
  left_join(year_reg_avgs, by = "release_year") %>%
  mutate(pred = mu + b_i + b_u + b_g + b_y) %>%
  pull(pred)

#Comparing the predicted ratings to the actual ratings and calculating a final RMSE:

round(RMSE(predicted_ratings, validation$rating),4)
