##### Data Science using R project #####
##### Movie Recommendation System using Item-based collaborative filtering #####
##### Authors : Shehyaaz Khan Nayazi, Shakshi Pandey, Riyanchhi Agrawal #####

#### Run this script first ! ####

# import libraries
library(data.table)
library(reshape2)
library(recommenderlab)

# Function to calculate ratingMatrix
calcSparseRatingMatrix <- function(ratings_data){
  #to convert our matrix into a sparse matrix one.
  ratingMatrix <- dcast(ratings_data, userId~movieId, value.var = "rating", na.rm=FALSE)
  ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
  #Convert rating matrix into a recommenderlab sparse matrix
  ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
  return(ratingMatrix)
}

# to calculate the weighted rating score of movies
weighted_rating <- function(data, m, C) {
  v <- data$movie_views
  R <- data$movie_rating
  return(v/(v+m)*R)+(m/(m+v)*C)
}

# Function to preprocess the dataset
pre_processing_data<- function(movies_data, ratings_data){
  movie_genre <- as.data.frame(movies_data$genres, stringsAsFactors=FALSE) # taking genre column
  movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], split="[|]", type.convert=TRUE), stringsAsFactors=FALSE)
  colnames(movie_genre2) <- c(1:10)
  
  # list of movie genres
  list_genre <- c("Action", "Adventure", "Animation", "Children", 
                  "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                  "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                  "Sci-Fi", "Thriller", "War", "Western")
  genre_mat1 <- matrix(0,dim(movie_genre)[1]+1,length(list_genre))
  genre_mat1[1,] <- list_genre # first row contains genre names
  colnames(genre_mat1) <- list_genre
  
  for (index in 1:nrow(movie_genre2)) {
    for (col in 1:ncol(movie_genre2)) {
      gen_col = which(genre_mat1[1,] == movie_genre2[index,col])
      genre_mat1[index+1,gen_col] <- 1
    }
  }
  genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
  for (col in 1:ncol(genre_mat2)) {
    genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
  } 
  #str(genre_mat2)
  
  # finding number of views and average rating for each movie
  ratingMatrix <- calcSparseRatingMatrix(ratings_data) # rows : userId , cols : movieId
  movie_views <- colCounts(ratingMatrix) # counting number of views for each movie
  movie_rating <- colMeans(ratingMatrix) # counting average rating for each movie
  length(movie_views) <- dim(movie_genre)[1]
  length(movie_rating) <- dim(movie_genre)[1]
  
  #‘PreprocessedMatrix’ that will allow us to perform an easy search of the films by specifying the genre present in our list
  PreprocessedMatrix <- cbind(movies_data[,1:2], genre_mat2, movie_views, movie_rating)
  
  # calculating Weighted Rating Score for demographic filtering
  C <- mean(movie_rating, na.rm = TRUE) # find mean movie rating
  m <- quantile(movie_views, probs = 0.9,na.rm=TRUE) # finding the 90th percentile of movie_views
  score <- weighted_rating(subset(PreprocessedMatrix, movie_views >= m), m, C)
  length(score) <- dim(movie_genre)[1]
  
  PreprocessedMatrix <- cbind(PreprocessedMatrix,score)
  # export PreprocessedMatrix
  write.csv(PreprocessedMatrix, file = "./pre-processed data/pre-processed-movies.csv", row.names = FALSE)
}

# set working directory
setwd("~/Documents/R-project/")
movies_data <- read.csv("./MovieLens dataset/movies.csv") # load movies data
ratings_data <- read.csv("./MovieLens dataset/ratings.csv") # load ratings data
pre_processing_data(movies_data, ratings_data)
