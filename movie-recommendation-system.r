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


pre_processing_data<- function(){
movie_genre <- as.data.frame(movies_data$genres, stringsAsFactors=FALSE)
library(data.table)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) #DataFlair
colnames(movie_genre2) <- c(1:10)

list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")
genre_mat1 <- matrix(0,10330,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre

for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col]) #Author DataFlair
    genre_mat1[index+1,gen_col] <- 1
  }
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
} 
str(genre_mat2)

#‘search matrix’ that will allow us to perform an easy search of the films by specifying the genre present in our list
SearchMatrix <- cbind(movies_data[,1:2], genre_mat2[])
print(head(SearchMatrix))    #DataFlair

#to convert our matrix into a sparse matrix one.
ratingMatrix <- dcast(ratings_data, userId~movieId, value.var = "rating", na.rm=FALSE)
ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
#Convert rating matrix into a recommenderlab sparse matrix
ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
print(ratingMatrix)

#implementing recommendation
recommendation_model <- recommenderRegistry$get_entries(dataType = "realRatingMatrix")
print(names(recommendation_model))
}

movies_data <- movies_dataset_details()
ratings_data <- ratings_dataset_details()
pre_processing <- pre_processing_data() 
