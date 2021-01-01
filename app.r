##### Data Science using R project #####
##### Movie Recommendation System using Item-based collaborative filtering #####
##### Authors : Shehyaaz Khan Nayazi, Shakshi Pandey, Riyanchhi Agrawal #####

# import libraries
library(recommenderlab)
library(data.table)
library(reshape2)
library(dplyr)
library(tidyr)
library(tibble)
library(purrr)
library(forcats)
library(ggplot2)

load_movies_dataset <- function(){
  movies_data <- read.csv("./MovieLens dataset/movies.csv") # load movies data
  return (movies_data)
}

load_ratings_dataset <- function(){
  ratings_data <- read.csv("./MovieLens dataset/ratings.csv") # load ratings data
  return (ratings_data)
}

load_preprocessed_dataset <- function(){
  preprocessed_data <- read.csv("./pre-processed data/pre-processed-movies.csv")
  return(preprocessed_data)
}

calcSparseRatingMatrix <- function(ratings_data){
  #to convert our matrix into a sparse matrix one.
  ratingMatrix <- dcast(ratings_data, userId~movieId, value.var = "rating", na.rm=FALSE)
  ratingMatrix <- as.matrix(ratingMatrix[,-1]) #remove userIds
  #Convert rating matrix into a recommenderlab sparse matrix
  ratingMatrix <- as(ratingMatrix, "realRatingMatrix")
  return(ratingMatrix)
}

# arrange the confusion matrix output for one model in a convenient format
avg_conf_mat <- function(results) {
  tmp <- results %>%
    getConfusionMatrix()  %>%  
    as.list() 
  as.data.frame(Reduce("+",tmp) / length(tmp)) %>% 
    mutate(n = c(1, 3, 5, 10, 15)) %>%
    select('n', 'precision', 'recall', 'TPR', 'FPR') 
}

# evaluate different recommender models
evaluate_models <- function(ratingMatrix){
  ratingMatrixNorm <- normalize(ratingMatrix)
  eval_sets <- evaluationScheme(data = ratingMatrixNorm,
                                method = "cross-validation",
                                train = 0.8, # 80% training data, 20% testing data
                                k = 5,
                                given = -1,
                                goodRating = 0)
  
  models_to_evaluate <- list(
    "IBCF Cosine" = list(name = "IBCF", 
                          param = list(method = "cosine")),
    "IBCF Pearson" = list(name = "IBCF", 
                          param = list(method = "pearson")),
    "UBCF Cosine" = list(name = "UBCF",
                          param = list(method = "cosine")),
    "UBCF Pearson" = list(name = "UBCF",
                          param = list(method = "pearson")),
    "Random" = list(name = "RANDOM", param=NULL)
  )
  
  n_recommendations <- c(1, 5, seq(10, 30, 10))
  
  list_results <- evaluate(x = eval_sets, 
                           method = models_to_evaluate, 
                           n = n_recommendations)
  result_table <- list_results %>%
    map(avg_conf_mat) %>% 
    # Turning into an unnested tibble
    enframe() %>%
    # Unnesting to have all variables on same level
    unnest()
  return(result_table)
}

# get movie_names
get_movie_details <- function(movie_data,ratingMatrix){
  ratingMatrix <- as(ratingMatrix,"matrix")
  movie_id <- as.integer(colnames(ratingMatrix))
  movie_names <- movie_data %>% 
    filter(movieId %in% movie_id) %>% pull(title) %>%
    as.character() %>% sort()
  return(list(movie_names = movie_names, movie_id = movie_id))
}

######## Shiny App ########

library(shiny)
library(shinydashboard)
library(shinyBS)
library(shinyWidgets)
library(DT)

# set working directory
setwd("~/Documents/R-project/")
# load required data
movies_data <- load_movies_dataset()
ratings_data <- load_ratings_dataset()
preprocessed_data <- load_preprocessed_dataset()
# calculate rating matrix
ratingMatrix <- calcSparseRatingMatrix(ratings_data)
# only movies with above 50 ratings and users who have rated more than 50 movies are considered
ratingMatrix <- ratingMatrix[rowCounts(ratingMatrix) > 50,
                             colCounts(ratingMatrix) > 50]  # considering only useful data in the dataset
movie_details <- get_movie_details(movies_data, ratingMatrix)

# set a seed
set.seed(123)

# evaluate Recommendation Models
eval_result <- evaluate_models(ratingMatrix)

# building IBCF Recommender Model
ratingMatrixNorm <- normalize(ratingMatrix)
scheme <- evaluationScheme(data = ratingMatrixNorm,
                              method = "cross-validation",
                              train = 0.8, # 80% training data, 20% testing data
                              k = 5, given = -1, goodRating = 0)
rec_mod <- Recommender(ratingMatrix, method = "IBCF", 
                       param = list(method = "pearson")) # recommendation model built


#### UI ####
ui <- dashboardPage(
  # dashboard header
  dashboardHeader(title = "Dashboard"),
  # dashboard sidebar
  dashboardSidebar(
    # sidebar menu
    sidebarMenu(
      menuItem(text = "Overview", tabName = "overview", icon = icon("home")),
      menuItem(text = "Movies dataset", tabName = "movies_dataset", icon = icon("film")),
      menuItem(text = "Ratings dataset", tabName = "ratings_dataset", icon = icon("star-half-alt")),
      menuItem(text = "Pre-processed dataset", tabName = "preprocessed_dataset", icon = icon("table")),
      menuItem(text = "Data Visualization", tabName = "data_visualization", icon = icon("chart-line")),
      menuItem(text = "Recommendation Models", tabName = "eval_recomm", icon = icon("clipboard-list")),
      menuItem(text = "DF Recommendation", tabName = "df_recomm", icon = icon("user-friends")),
      menuItem(text = "IBCF Recommendation", tabName = "ibcf_recomm", icon = icon("user-alt"))
    )
  ),
  # dashboard body
  dashboardBody(
    tabItems(
      tabItem(
        tabName = "overview",
        fluidPage(
         h1("Recommendation using Collaborative Filtering"), # overview of collaborative filtering
         box(
           width = 12,
           h4(
             "A recommendation system provides suggestions to the users through a filtering process 
             that is based on user preferences and browsing history.To build a recommender system, 
             the most two popular approaches are Content-based and Collaborative Filtering."
           ),
           br(),
           h4(
             "Content-based approach requires a good amount of information of items’ own features, 
             rather than using users’ interactions and feedbacks. Collaborative Filtering, on the 
             other hand, doesn’t need anything else except users’ historical preference on a set of 
             items. Because it’s based on historical data, the core assumption here is that the users 
             who have agreed in the past tend to also agree in the future."
           ),
           br(),
           h4(
             "This project uses the MovieLens dataset which contains ratings of more than 10,000 movies 
             rated by more than 600 users. Movies are recommended to users using Demographic Filtering 
             and Item-based Collaborative Filtering."
           )
         )
        )
      ),
      tabItem(
        tabName = "movies_dataset",
        fluidPage(
          h1("Movies Dataset"),
          dataTableOutput("movies_data")
          ) # describe movies dataset
      ),
      tabItem(
        tabName = "ratings_dataset",
        fluidPage(
          h1("Ratings Dataset"),
          dataTableOutput("ratings_data")
          ) # describe ratings dataset
      ),
      tabItem(
        tabName = "preprocessed_dataset",
        fluidPage(
          h1("Preprocessed Movies Dataset"),
            dataTableOutput("preprocessed_data")
          ) # describe movies dataset
      ),
      tabItem(
        tabName = "data_visualization",
        fluidPage(
          fluidRow(
            box(
              title = "Histogram of Average Movie Ratings",
              status = "warning", solidHeader = TRUE, collapsible = TRUE,
              width = 12,
              plotOutput("avg_movie_rating")
            )
          ),
          fluidRow(
            box(
              title = "Histogram of Average User Ratings",
              status = "warning", solidHeader = TRUE, collapsible = TRUE,
              width = 6,
              plotOutput("avg_user_rating")
            ),
            box(
              title = "Histogram of Normalized Average User Ratings",
              status = "warning", solidHeader = TRUE, collapsible = TRUE,
              width = 6,
              plotOutput("norm_avg_user_rating")
            )
          ),
          fluidRow(
            box(
              title = "Scatterplot of Number of Reviews vs Average Movie Ratings",
              status = "warning", solidHeader = TRUE, collapsible = TRUE,
              width = 12,
              plotOutput("views_rating")
            )
          ),
          fluidRow(
            box(
              title = "Heatmap of User Dissimilarity",
              status = "warning", solidHeader = TRUE, collapsible = TRUE,
              width = 6,
              sliderInput(
                inputId = "num_users", min = 4, max = 20, value = 10, label = "Number of Users"
              ),
              plotOutput("user_similarity")
            ),
            box(
              title = "Heatmap of Movie Dissimilarity",
              status = "warning", solidHeader = TRUE, collapsible = TRUE,
              width = 6,
              sliderInput(
                inputId = "num_movies", min = 4, max = 20, value = 10, label = "Number of Movies"
              ),
              plotOutput("movie_similarity")
            )
          )
        )
      ),
      tabItem(
        tabName = "eval_recomm",
        fluidPage(
          h1("Evaluation of Different Recommendation Algorithms"),
          fluidRow(
            box(
              title = "ROC Curve",
              status = "warning", solidHeader = TRUE, collapsible = TRUE,
              width = 12,
              plotOutput("eval_roc")
            )
          ),
          fluidRow(
            box(
              title = "Precision-Recall Curve",
              status = "warning", solidHeader = TRUE, collapsible = TRUE,
              width = 12,
              plotOutput("eval_pr")
            )
          )
        )
      ),
      tabItem(
        tabName = "df_recomm",
        fluidPage(
          h1("Recommended Movies based on Demographic Filtering"),
          sliderInput(
            inputId = "num_recomm_df", min = 5, max = 50, value = 20, label = "Number of Recommendations"
          ),
          h4("Recommendend Movies"),
          dataTableOutput("df_movies")
        )
      ),
      tabItem(
        tabName = "ibcf_recomm",
        fluidPage(
          h1("Recommended Movies based on Item-based Collaborative Filtering"),
          h3("Select movies and ratings (maximum of 10 selections)"),
          pickerInput(inputId = "movie_selection",
                      label = "",
                      choices = movie_details$movie_names,
                      selected = movie_details$movie_names[1:2],
                      options = pickerOptions(
                        actionsBox = FALSE,
                        liveSearch = TRUE,
                        maxOptions = 10 # maximum of options
                      ), 
                      multiple = TRUE),
          h4(" "),
          uiOutput("movie_rating01"),
          uiOutput("movie_rating02"),
          uiOutput("movie_rating03"),
          uiOutput("movie_rating04"),
          uiOutput("movie_rating05"),
          uiOutput("movie_rating06"),
          uiOutput("movie_rating07"),
          uiOutput("movie_rating08"),
          uiOutput("movie_rating09"),
          uiOutput("movie_rating10"),
          actionButton("run", "Recommend using IBCF"),
          actionButton("runHybrid", "Recommend using Hybrid Filtering"),
          br(),
          h3("Top 10 Movie recommendations for you"),
          dataTableOutput("recomm")
        )
      )
    )
  )
  
) # end of UI

#### Server ####
server <- function(input, output, session) {
  # display sliders for movie ratings
  output$movie_rating01 <- renderUI({
    if(length(input$movie_selection) > 0){
      fluidRow(
        column(
          shinyBS::popify(
            el = sliderInput(inputId = input$movie_selection[1],
                             label = input$movie_selection[1],
                             max = 5, min = 0, step = 0.5, value = 3),
            title = "",
            placement = "right"
          ),
          width = 10
        )
      )
    }
  })
  
  output$movie_rating02 <- renderUI({
    if(length(input$movie_selection) > 1){
      fluidRow(
        column(
          shinyBS::popify(
            el = sliderInput(inputId = input$movie_selection[2],
                             label = input$movie_selection[2],
                             max = 5, min = 0, step = 0.5, value = 3),
            title = "",
            placement = "right"
          ),
          width = 10
        )
      )
    }
  })
  
  output$movie_rating03 <- renderUI({
    if(length(input$movie_selection) > 2){
      fluidRow(
        column(
          shinyBS::popify(
            el = sliderInput(inputId = input$movie_selection[3],
                             label = input$movie_selection[3],
                             max = 5, min = 0, step = 0.5, value = 3),
            title = "",
            placement = "right"
          ),
          width = 10
        )
      )
    }
  })
  
  output$movie_rating04 <- renderUI({
    if(length(input$movie_selection) > 3){
      fluidRow(
        column(
          shinyBS::popify(
            el = sliderInput(inputId = input$movie_selection[4],
                             label = input$movie_selection[4],
                             max = 5, min = 0, step = 0.5, value = 3),
            title = "",
            placement = "right"
          ),
          width = 10
        )
      )
    }
  })
  
  output$movie_rating05 <- renderUI({
    if(length(input$movie_selection) > 4){
      fluidRow(
        column(
          shinyBS::popify(
            el = sliderInput(inputId = input$movie_selection[5],
                             label = input$movie_selection[5],
                             max = 5, min = 0, step = 0.5, value = 3),
            title = "",
            placement = "right"
          ),
          width = 10
        )
      )
    }
  })
  
  output$movie_rating06 <- renderUI({
    if(length(input$movie_selection) > 5){
      fluidRow(
        column(
          shinyBS::popify(
            el = sliderInput(inputId = input$movie_selection[6],
                             label = input$movie_selection[6],
                             max = 5, min = 0, step = 0.5, value = 3),
            title = "",
            placement = "right"
          ),
          width = 10
        )
      )
    }
  })
  
  output$movie_rating07 <- renderUI({
    if(length(input$movie_selection) > 6){
      fluidRow(
        column(
          shinyBS::popify(
            el = sliderInput(inputId = input$movie_selection[7],
                             label = input$movie_selection[7],
                             max = 5, min = 0, step = 0.5, value = 3),
            title = "",
            placement = "right"
          ),
          width = 10
        )
      )
    }
  })
  
  output$movie_rating08 <- renderUI({
    if(length(input$movie_selection) > 7){
      fluidRow(
        column(
          shinyBS::popify(
            el = sliderInput(inputId = input$movie_selection[8],
                             label = input$movie_selection[8],
                             max = 5, min = 0, step = 0.5, value = 3),
            title = "",
            placement = "right"
          ),
          width = 10
        )
      )
    }
  })
  
  output$movie_rating09 <- renderUI({
    if(length(input$movie_selection) > 8){
      fluidRow(
        column(
          shinyBS::popify(
            el = sliderInput(inputId = input$movie_selection[9],
                             label = input$movie_selection[9],
                             max = 5, min = 0, step = 0.5, value = 3),
            title = "",
            placement = "right"
          ),
          width = 10
        )
      )
    }
  })
  
  output$movie_rating10 <- renderUI({
    if(length(input$movie_selection) > 9){
      fluidRow(
        column(
          shinyBS::popify(
            el = sliderInput(inputId = input$movie_selection[10],
                             label = input$movie_selection[10],
                             max = 5, min = 0, step = 0.5, value = 3),
            title = "",
            placement = "right"
          ),
          width = 10
        )
      )
    }
  })
  
  # get user data
  recomdata <- reactive({
    selected_movies <- movies_data %>%
      filter(movieId %in% movie_details$movie_id) %>%
      filter(title %in% input$movie_selection) %>%
      arrange(title) %>%
      select(-c(genres))
    new_movies <- movies_data %>% filter(movieId %in% movie_details$movie_id)
    
    for(i in 1:nrow(selected_movies)){
      selected_movies$ratingvec[i] <- input[[as.character(selected_movies$title[i])]]
    }
    rating_vec <- new_movies %>% left_join(., selected_movies, by = "movieId") %>% 
      pull(ratingvec)
    rating_vec <- as.matrix(t(rating_vec))
    rating_vec <- as(rating_vec, "realRatingMatrix")
    top_n_prediction <- predict(rec_mod, rating_vec, n = 10)
    top_n_list <- as(top_n_prediction, "list")
    top_n_df <- data.frame(top_n_list)
    colnames(top_n_df) <- "movieId"
    top_n_df$movieId <- as.numeric(levels(top_n_df$movieId))
    names <- left_join(top_n_df, movies_data, by="movieId")
    names <- as.data.frame(names) %>%select(-c(movieId, genres)) %>% 
      rename(Title = title)
    names
  })
  
  # get data for hybrid filtering
  recomdataHybrid <- reactive({
    selected_movies <- movies_data %>%
      filter(movieId %in% movie_details$movie_id) %>%
      filter(title %in% input$movie_selection) %>%
      arrange(title) %>%
      select(-c(genres))
    new_movies <- movies_data %>% filter(movieId %in% movie_details$movie_id)
    
    for(i in 1:nrow(selected_movies)){
      selected_movies$ratingvec[i] <- input[[as.character(selected_movies$title[i])]]
    }
    rating_vec <- new_movies %>% left_join(., selected_movies, by = "movieId") %>% 
      pull(ratingvec)
    rating_vec <- as.matrix(t(rating_vec))
    rating_vec <- as(rating_vec, "realRatingMatrix")
    top_n_prediction <- predict(rec_mod, rating_vec, n = 10)
    top_n_list <- as(top_n_prediction, "list")
    top_n_df <- data.frame(top_n_list)
    colnames(top_n_df) <- "movieId"
    top_n_df$movieId <- as.numeric(levels(top_n_df$movieId))
    names <- left_join(top_n_df, preprocessed_data, by="movieId")
    names <- as.data.frame(names) %>%select(c(title, score)) %>% arrange(desc(score)) %>%
      rename(Title = title)
    names
  })
  
  # display tables, graphs, and results
  output$movies_data <- renderDataTable(movies_data)
  output$ratings_data <- renderDataTable(ratings_data)
  output$preprocessed_data <- renderDataTable(preprocessed_data,  options = list(scrollX = TRUE))
  output$avg_movie_rating <- renderPlot({
    ggplot(preprocessed_data, aes(x = movie_rating))+
      geom_histogram(bins = 100, na.rm = TRUE)+
      ggtitle("Histogram - Average Movie Rating")+xlab("Average Movie Rating")+ylab("Frequency")
  })
  output$avg_user_rating <- renderPlot({
    user_ratings <- rowMeans(ratingMatrix)
    qplot(x = user_ratings, fill=I("steelblue"), xlab = "Average User Rating", ylab = "Frequency",
          main = "Histogram of Average User Rating")
  })
  output$norm_avg_user_rating <- renderPlot({
    normalized_rating <- normalize(ratingMatrix)
    user_ratings <- rowMeans(normalized_rating)
    qplot(x = user_ratings, fill=I("blue"), xlab = "Normalized Average User Rating", ylab = "Frequency",
          main = "Histogram of Normalized Average User Rating")
  })
  output$views_rating <- renderPlot({
    ggplot(preprocessed_data, aes(x = movie_views, y = movie_rating))+
      geom_point(size = 2, color = "orange", na.rm = TRUE)+
      geom_smooth(method = "lm", color = "red", na.rm = TRUE)+
      ggtitle("Movie Reviews vs Average Movie Rating")+xlab("Number of Reviews")+ylab("Average Movie Rating(out of 5)")
  })
  output$user_similarity <- renderPlot({
    users_simi <- similarity(ratingMatrix[1:input$num_users,], method = "cosine", which = "users")
    users_simi <- as.matrix(users_simi)
    ggplot(data = melt(users_simi), aes(x = as.character(Var1), y = as.character(Var2), fill = value))+
      geom_tile()+xlab("User Id")+ylab("User Id")
  })
  output$movie_similarity <- renderPlot({
    movie_simi <- similarity(ratingMatrix[,1:input$num_movies], method = "cosine", which = "items")
    movie_simi <- as.matrix(movie_simi)
    ggplot(data = melt(movie_simi), aes(x = as.character(Var1), y = as.character(Var2), fill = value))+
      geom_tile()+xlab("Movie Id")+ylab("Movie Id")
  })
  # roc curve
  output$eval_roc <- renderPlot({
      ggplot(eval_result, aes(FPR, TPR, colour = fct_reorder2(as.factor(name), FPR, TPR))) +
      geom_line() +
      geom_label(aes(label = n))  +
      labs(title = "ROC curves", colour = "Model") +
      theme_grey(base_size = 14)
  })
  # precision - recall curve
  output$eval_pr <- renderPlot({
    ggplot(eval_result, aes(recall, precision, colour = fct_reorder2(as.factor(name), precision, recall))) +
      geom_line() +
      geom_label(aes(label = n))  +
      labs(title = "Precision-Recall curves", colour = "Model") +
      theme_grey(base_size = 14)
  })
  
  # recommended movies - df
  df <- preprocessed_data[order(preprocessed_data$score, decreasing = TRUE),]
  df <- df %>% select(c(title, score))
  row.names(df) <- NULL
  output$df_movies <- renderDataTable({
   df[1:input$num_recomm_df, ]
  })
  
  # recommended movies - ibcf
  observeEvent(input$run, {
    if(length(input$movie_selection) == 0){
      sendSweetAlert(
        session = session,
        title = "Please select more movies.",
        text = "Rate at least two movies.",
        type = "info")
    }else{
      recomdata <- recomdata()
      if(length(input$movie_selection) < 2){
        sendSweetAlert(
          session = session,
          title = "Please select more movies.",
          text = "Rate at least two movies.",
          type = "info")
      }
      else if(nrow(recomdata) < 1){
        sendSweetAlert(
          session = session,
          title = "Please vary in your ratings.",
          text = "Do not give the same rating for all movies.",
          type = "info")
      } else{
        output$recomm <- renderDataTable(recomdata) 
      }
    }
  })
  
  # recommended movies - Hybrid - combination of IBCF and demographic filtering
  observeEvent(input$runHybrid, {
    if(length(input$movie_selection) == 0){
      sendSweetAlert(
        session = session,
        title = "Please select more movies.",
        text = "Rate at least two movies.",
        type = "info")
    }else{
      recomdata <- recomdataHybrid()
      if(length(input$movie_selection) < 2){
        sendSweetAlert(
          session = session,
          title = "Please select more movies.",
          text = "Rate at least two movies.",
          type = "info")
      }
      else if(nrow(recomdata) < 1){
        sendSweetAlert(
          session = session,
          title = "Please vary in your ratings.",
          text = "Do not give the same rating for all movies.",
          type = "info")
      } else{
        output$recomm <- renderDataTable(recomdata) 
      }
    }
  })
} # end of server function

#### Run the Shiny App ####
shinyApp(ui = ui, server = server)
