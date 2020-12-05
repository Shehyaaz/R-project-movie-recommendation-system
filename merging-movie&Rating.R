# merging two data frame
Movies<- read.csv(file = 'movies.csv')
head(Movies)
Ratings <- read.csv(file = 'ratings.csv')
head(Ratings)
MovieRecommedation<-merge(Ratings, Movies, by.x = 'movieId', by.y = 'movieId')
write.csv(MovieRecommedation,"movie-recomm.csv",row.names = FALSE)

dd <- MovieRecommedation # data

dd<-dd %>% group_by(rating)  %>% mutate(count = n())
write.csv(df,"movie-recco.csv",row.names = FALSE)
df
library(dplyr)

#count number of ratings
dd<-dd %>% group_by(movieId)  %>% mutate(No_OfRatings = n())
dd
dd %>% 
  group_by(movieId) %>% 
  mutate(avgRating = sum(rating)/No_OfRatings)

# demographic score

c<-mean(dd$rating,na.rm=TRUE)
c
m<-quantile(dd$rating, probs = 0.9,na.rm=TRUE)
m
filter(dd,rating >= m)
dd
weighted_rating <- function(m=m,c=c) {
  v<-dd$rating
  R<-dd$No_OfRatings
  return(v/(v+m)*R)+(m/(m+v)*c)
}
s<-weighted_rating(m,c)
s
s<-s*100
s
#s<-order(s, decreasing=TRUE)
format(round(s, 2), nsmall = 2)

 dd%>% 
  group_by(movieId) %>% 
  mutate(score =as.double(s) )
 dd[order(dd$score, decreasing = TRUE),]
 head(dd)
 