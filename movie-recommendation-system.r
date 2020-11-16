"
##### Data Science using R project #####
##### Movie Recommendation System using Item-based collaborative filtering #####
##### Authors : Shehyaaz Khan Nayazi, Shakshi Pandey, Riyanchhi Agrawal #####
"
# Installing required libraries
# install.packages("recommenderlab")
# install.packages("data.table")
# install.packages("reshape2")
# install.packages("ggplot2")
# import libraries
library(recommenderlab)
library(data.table)
library(reshape2)
library(ggplot2)

# set working directory
setwd("~/Documents/R-project/")

movies_dataset_details <- function(){
  movies_data <- read.csv("./IMDB dataset/movies.csv") # load movies data
  # display dataset details
  print("Movies data-frame :")
  print(head(movies_data))
  print("Summary of Movies data-frame :")
  print(summary(movies_data))
  # return the dataset
  return (movies_data )
}

ratings_dataset_details <- function(){
  ratings_data <- read.csv("./IMDB dataset/ratings.csv") # load ratings data
  # display dataset details
  print("Ratings data-frame :")
  print(head(ratings_data))
  print("Summary of Ratings data-frame :")
  print(summary(ratings_data))
  # return the dataset
  return (ratings_data)
}

movies_data <- movies_dataset_details()
ratings_data <- ratings_dataset_details()
