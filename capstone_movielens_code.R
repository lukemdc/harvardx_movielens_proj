#' ---
#' title: "HarvardX MovieLens Final Project"
#' author: "Luke M."
#' date: "23 June, 2020"
#' ---
#' 

######################### 
#Data Cleaning/Setup
######################### 
# !diagnostics off

if(!require(klaR)) install.packages("klar", repos = "http://cran.us.r-project.org")
if(!require(matrixStats)) install.packages("matrixStats", repos = "http://cran.us.r-project.org")
if(!require(knitr)) install.packages("knitr", repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", repos = "http://cran.us.r-project.org")
if(!require(randomForest)) install.packages("randomForest", repos = "http://cran.us.r-project.org")
if(!require(stringr)) install.packages("stringr", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", repos = "http://cran.us.r-project.org")

start.time <- Sys.time()
set.seed(1) # use `set.seed(1, sample.kind = "Rounding")` in R >=3.6 

#load data from "data" folder on C drive of computer
edx<-readRDS("C:/data/edx.rds")
validation<-readRDS("C:/data/validation.rds")

#code below creates column with the movie's year and rating's year
edx[["movie_year"]] <- as.numeric(str_sub(edx$title,-5,-2))
edx<-edx %>% mutate(rating_year = year(as_datetime(timestamp)))
validation[["movie_year"]] <- as.numeric(str_sub(validation$title,-5,-2))
validation <- validation %>% mutate(rating_year = year(as_datetime(timestamp)))

#remove all dashes in the genres column. this will help our code run smoothly late
edx$genres<-gsub("-", "", edx$genres)
validation$genres<-gsub("-", "", validation$genres)

#convert several of the columns to factors to save space and make them more efficient
edx[["movieId"]]<-as.factor(edx[["movieId"]])
edx[["title"]]<-as.factor(edx[["title"]])
edx[["genres"]]<-as.factor(edx[["genres"]])

#repeat the process for the validation data, converting key columns to factors to reduce storage space
validation[["movieId"]]<-as.factor(validation[["movieId"]])
validation[["title"]]<-as.factor(validation[["title"]])
validation[["genres"]]<-as.factor(validation[["genres"]])

#change the factors of the validation set so they match the factor levels of the edx set
levels(validation$movieId)<-levels(edx$movieId)
levels(validation$title)<-levels(edx$title)
levels(validation$genres)<-levels(edx$genres)

#create a list of the entries in the genres column
genre_levels<-levels(as.factor(edx$genres))

#split this list into a list of individiual genres, instead of groups of genres separated by a "|" character
splits<-sapply(genre_levels, function(g){
  strsplit(g,"[|]")[[1]]
})

#remove duplicate genres from the list created earlier 
genres<-unique(unlist(splits, use.names=F))

#create a matrix listing each unique movie and the genres that apply
titles_genres<-distinct(select(edx,c("title","genres")),title, .keep_all = T)
genres_matrix<-data.frame(unique(titles_genres$title))
names(genres_matrix)<-"title"
for (genre in genres){
  #append a new column that indicates if the movie in each row is in the genre
  genres_matrix[[genre]] <-str_detect(titles_genres$genres,genre)
}

#delete the column with "no genres listed"
genres_matrix<- suppressMessages(select(genres_matrix,-c("(no genres listed)")))

#append the boolean genre columns to edx and validation datasets
edx <- left_join(edx, genres_matrix, by="title")
validation <- left_join(validation, genres_matrix, by="title")


#########################
#Get descriptive statistics
#########################
#create histogram of ratings for edx dataset
ggplot(edx,aes(rating)) +
  geom_histogram(binwidth = .5) +
  labs(x="Rating",y="Count",title="Distribution of Ratings in the Edx and Validation Datasets") +
  theme_grey()

#create histogram of ratings for validation dataset
ggplot(validation,aes(rating)) +
  geom_histogram(binwidth = .5) +
  labs(x="Rating",y="Count",title="Distribution of Ratings in the Edx and Validation Datasets") +
  theme_grey()

#get highest rated genre groups
highest_rated <- edx %>% group_by(genres) %>% summarise(avg_rating=mean(rating), n=n()) %>% 
  arrange(desc(avg_rating))
kable(highest_rated[1:10,])
#get lowest rated genre groups
lowest_rated <- edx %>% group_by(genres) %>% summarise(avg_rating=mean(rating), n=n()) %>% 
  arrange(avg_rating)
kable(lowest_rated[1:10,])

#delete "no genres listed" from genres list
genres<-genres[which(genres!="(no genres listed)")]

#create a table with the average rating, sd of the ratings, and number of ratings per genre
ratings<-sapply(genres,function(genre){
  print(paste("Getting descriptive statistics for", genre, "genre."))
  edx %>% filter(str_detect(genres,genre)==1) %>% summarize(mean=mean(rating), sd=sd(rating),n=n())
})

#convert the table to a dataframe for easy retrieval 
ratings<-data.frame(ratings)
ratings["genre",]<-names(ratings)

#transpose the descriptive statistics so we can sort it more easily from highest to lowest mean rating
ratings_sortable<-data.frame()
for (rating_ in ratings){
  ratings_sortable<-rbind(ratings_sortable,rating_)
}

#print a table with the genres from highest to lowest average rating
ratings_table<-ratings_sortable %>% arrange(desc(mean))
#kable(ratings_table, caption="Descriptive Statistics by Genre")

#create graph showing average, SD, and number of the ratings for each genre
ggplot(ratings_table, aes(x=reorder(genre, -mean), y=mean, color=reorder(genre, -mean))) +
  geom_point(aes(size=n), show.legend = F) +
  geom_hline(aes(yintercept = mean(edx$rating)), color="red") +
  geom_errorbar(aes(ymin = mean-sd, ymax = mean+sd), width = 0.5, size = 0.5, show.legend=F) +
  labs(x="Genre", y="Average Rating and SD", title="Average Ratings, SD, and Number of Observations by Genre") +
  theme(axis.text.x = element_text(angle = 90))

#create copy of the genres list
genres_<-genres

#calculate  pearson pair-wise correlations between each genre and ratings
genres_ratings_correlations<-t(cor(edx$rating,select(edx,genres_), method = 'pearson')) 

#reformat the correlation coefficients into a sorted table
genres_ratings_correlations<- data.frame(genre=rownames(genres_ratings_correlations),correlation_coeff=genres_ratings_correlations[,1]) %>% 
  mutate(rank=rank(desc(correlation_coeff))) %>%
  arrange(rank)
kable(genres_ratings_correlations, caption="Correlation Coefficients for Genre Correlations to Ratings")

#get list of genres to drop from the dataframe because they are too weakly correlated with ratings
genres_to_drop <- genres_ratings_correlations %>% filter(abs(correlation_coeff)<.02) %>% .$genre
genres_to_keep <- genres_ratings_correlations %>% filter(abs(correlation_coeff)>=.02) %>% .$genre

edx <- select(edx, -c(all_of(genres_to_drop)))
validation <- select(validation, -c(all_of(genres_to_drop)))

#clean up the workspace
rm(genres, rating_, genres_ratings_correlations, ratings_sortable, ratings, splits, genre, genre_levels)

#create train and test sets balanced on movie title. you'll get a message warning that some movie titles only have one rating
#this will run without warnings; the supressWarnings method call just hides the printout
train_index <- suppressWarnings(createDataPartition(edx$title, times = 1, p=.8, list = F))
train <- edx[train_index,]
test <- edx[-train_index,]

#remove edx and genre_matrix objects from global environment to save memory space
rm(edx, train_index, titles_genres, highest_rated, lowest_rated)

#add columns for the movie's average rating, the user's average rating, and a measure of popularity,
#which is calculated as the log of number of ratings*sum or all ratings
train <- train %>% group_by(title) %>% mutate(movie_avg=mean(rating))  
train <- train %>% group_by(userId) %>% mutate(user_avg=mean(rating))

#########################
#Baseline models for performance comparison 
#########################

#create empty tables where rmse results for each model will be stored for easy comparison
train_rmse_table<-data.frame(model=character(0),rmse=double(0))
test_rmse_table<-data.frame(model=character(0),rmse=double(0))
validation_rmse_table<-data.frame(model=character(0),rmse=double(0))

#create results dataframes where the y-hats from each model will be stored for later comparison
train_results<-data.frame(title=train$title,rating=train$rating)
test_results<-data.frame(title=test$title,rating=test$rating)
validation_results<-select(validation,c(title,userId,rating))

####random guessing method
#create list of possible ratings
poss_ratings<-seq(.5,5,.5)

#use random sample of possible ratings to generate, predicted ratings for the train, test, and validation sets and append the results to the RMSE tables for each respective dataset
train_results[["y_hat_random_guesses"]]<-sample(poss_ratings,nrow(train),replace = T)
train_rmse_table[nrow(train_rmse_table)+1,]<-c("random_guesses",RMSE(train_results$y_hat_random_guesses,train_results$rating))

test_results[["y_hat_random_guesses"]]<-sample(poss_ratings,nrow(test),replace = T)
test_rmse_table[nrow(test_rmse_table)+1,]<-c("random_guesses",RMSE(test_results$y_hat_random_guesses,test_results$rating))

validation_results[["y_hat_random_guesses"]]<-sample(poss_ratings,nrow(validation),replace = T)
validation_rmse_table[nrow(validation_rmse_table)+1,]<-c("random_guesses",RMSE(validation_results$y_hat_random_guesses,validation_results$rating))

####weighted guessing method
#estimate the proportion of observations in the training data that have each rating level
probs<-train %>% group_by(rating) %>% summarize(prob_per_rating=as.double(n()/nrow(train)))

#assign predictions at each rating level according their prior known probabilities estimated from the training set, and then estimate the RMSEs and append the RMSEs and predictions to the RMSE and results tables 
train_results[["y_hat_weighted_guesses"]]<-sample(poss_ratings,nrow(train),replace = T, prob=c(probs$prob_per_rating))
train_rmse_table[nrow(train_rmse_table)+1,]<-c("weighted_guesses",RMSE(train_results$y_hat_weighted_guesses,train_results$rating))

test_results[["y_hat_weighted_guesses"]]<-sample(poss_ratings,nrow(test),replace = T, prob=c(probs$prob_per_rating))
test_rmse_table[nrow(test_rmse_table)+1,]<-c("weighted_guesses",RMSE(test_results$y_hat_weighted_guesses,test_results$rating))

validation_results[["y_hat_weighted_guesses"]]<-sample(poss_ratings,nrow(validation),replace = T, prob=c(probs$prob_per_rating))
validation_rmse_table[nrow(validation_rmse_table)+1,]<-c("weighted_guesses",RMSE(validation_results$y_hat_weighted_guesses,validation_results$rating))

####global average method
#make predictions solely based on the overall average rating of all movies in the training dataset
train_results[["y_hat_global_mean"]]<-mean(train$rating)
train_rmse_table<-rbind(train_rmse_table,c("global_mean",RMSE(train_results$y_hat_global_mean,train_results$rating)))

test_results[["y_hat_global_mean"]]<-mean(train$rating)
test_rmse_table<-rbind(test_rmse_table,c("global_mean",RMSE(test_results$y_hat_global_mean,test_results$rating)))

validation_results[["y_hat_global_mean"]]<-mean(train$rating)
validation_rmse_table<-rbind(validation_rmse_table,c("global_mean",RMSE(validation_results$y_hat_global_mean,validation_results$rating)))

####movie averages method
#predict ratings based on the movie's average rating in the training dataset 
train_results[["y_hat_movie_mean"]]<-train$movie_avg
train_rmse_table<-rbind(train_rmse_table,c("movie_mean",RMSE(train_results$y_hat_movie_mean,train_results$rating)))

#create table listing each movie's average rating in the training dataset
avg_ratings<-train %>% ungroup() %>% select(c("movie_avg","title"))
avg_ratings<-distinct(avg_ratings)

#predict ratings in the test set based on each movie's average ratings in the training data
test <- left_join(test, avg_ratings, by="title")
test_results<-left_join(test_results, avg_ratings, by="title")

#rename the column to match naming convention used in the results tables
colnames(test_results)[which(colnames(test_results) == 'movie_avg')] <- 'y_hat_movie_mean'
test_rmse_table<-rbind(test_rmse_table,c("movie_mean",RMSE(test_results$y_hat_movie_mean,test_results$rating)))

#repeat this prediction method for the validation set 
validation <- left_join(validation, avg_ratings, by="title")
validation_results <- left_join(validation_results, avg_ratings, by="title")

#if any movies don't have average ratings from the training data, predict rating based on overall average rating from the training observations
validation$movie_avg[is.na(validation$movie_avg)] <-mean(train$rating)
validation_results$movie_avg[is.na(validation_results$movie_avg)] <-mean(train$rating)

#rename the column to match naming convention used in the results tables
names(validation_results)[which(names(validation_results) == 'movie_avg')] <- 'y_hat_movie_mean'
validation_rmse_table<-rbind(validation_rmse_table,c("movie_mean",RMSE(validation_results$y_hat_movie_mean,validation_results$rating)))

#we will repeat this method to merge the user_avg data and use userId as the merge key
user_avgs<-train %>% ungroup() %>% select(c("user_avg", "userId"))
user_avgs<-distinct(user_avgs)
test <- left_join(test, user_avgs, by="userId")
test$user_avg[is.na(test$user_avg)] <- mean(train$rating)
validation <- left_join(validation, user_avgs, by="userId")
validation$user_avg[is.na(validation$user_avg)] <- mean(train$rating)

rm(probs,poss_ratings)

#############
#Regularization
#############
#save the average rating from the training set as overall_mean 
overall_mean<-mean(train$rating)

#create a list of lambdas to test
lambdas <- seq(4, 6, .25)

#use sapply to create a list of estimated RMSEs achieved by regularization using each lambda
regularization_results<-sapply(lambdas,function(lambda){
  print(paste("Testing regularization with lambda =",lambda))
  
  #create list of regularized movie effects
  reg_effects <<- train %>% 
    group_by(title) %>% 
    #all column for the overall mean rating
    mutate(overall_mean=overall_mean) %>%
    mutate(movie_effect = sum(rating - overall_mean)/(n()+lambda))
  
  #create list of regularized user effects
  reg_effects <<- reg_effects %>% 
    group_by(userId) %>% 
    mutate(user_effect = sum(rating - overall_mean - movie_effect)/(n()+lambda))  
  
  #add list of regularized genre effects by looping through the list of genres
  reg_effects_copy <<- reg_effects
  genre_effects <<- data.frame(matrix(NA,ncol=1,nrow=nrow(train)))[-1]
  for (genre in genres_to_keep){
    #sum all of the genre effects that have been calculated so far
    reg_effects_copy$cumulative_genre_effects<-rowSums(genre_effects)
    
    #estimate the effect of the genre currently in the loop
    genre_eff <- reg_effects_copy %>% 
      group_by_(genre) %>% 
      mutate(genre_effect = sum(rating - overall_mean - movie_effect - user_effect - cumulative_genre_effects)/(n()+lambda)) %>%
      .$genre_effect
    
    #update the genre_effects matrix which has genre effects for each genre looped through so far
    genre_effects<<-cbind.data.frame(genre_effects,genre_eff)
  }
  
  #create column that sums together all of the estimated genre effects
  reg_effects$cumulative_genre_effects<-rowSums(genre_effects)
  
  #create data frame with just the movie effects and genre effects for each movie
  movie_adjusments <- reg_effects %>% ungroup() %>% select(c(title, movie_effect, cumulative_genre_effects))
  movie_adjusments <- distinct(movie_adjusments)
  
  #create data frame with the user effects for each user
  user_adjustments <- reg_effects %>% ungroup() %>% select(c(userId, user_effect))
  user_adjustments <- distinct(user_adjustments)
  
  #bind the movie_adjustments and user_adjustments to the test set and validation sets
  test_copy <- test %>% left_join(movie_adjusments, by="title") %>% left_join(user_adjustments, by="userId")
  validation_copy <- validation %>% left_join(movie_adjusments, by="title") %>% left_join(user_adjustments, by="userId")
  
  #calculate y_hat for the test set
  y_hat_reg_train <- overall_mean + reg_effects$user_effect + reg_effects$movie_effect + reg_effects$cumulative_genre_effects
  y_hat_reg_test <- overall_mean + test_copy$user_effect + test_copy$movie_effect + test_copy$cumulative_genre_effects
  y_hat_reg_validation <- overall_mean + validation_copy$user_effect + validation_copy$movie_effect + validation_copy$cumulative_genre_effects
  
  #print the results to the console
  print(paste("The RMSE for the train set is", RMSE(y_hat_reg_train,reg_effects$rating)))
  print(paste("The RMSE for the train set is", RMSE(y_hat_reg_test,test$rating)))
  print(paste("The RMSE for the train set is", RMSE(y_hat_reg_validation,validation$rating)))
  
  list(lambda,RMSE(y_hat_reg_test,test$rating),y_hat_reg_train,y_hat_reg_test,y_hat_reg_validation)
}) 

#save the lambda that achieved the lowest RMSE
best_lambda<-unlist(regularization_results[1,which.min(regularization_results[2,])])
print(paste("The best lambda for regularization is",best_lambda, "with RMSE=",unlist(regularization_results[2,which.min(regularization_results[2,])])))

#store lambdas and RMSEs in a table for easy graphing
lambda_tune <- data.frame("lambda"=unlist(regularization_results[1,]),
                          "RMSE"=unlist(regularization_results[2,]))

###print this
#plots results to determine the best lambda
ggplot(lambda_tune, aes(x=lambda, y=RMSE)) +
  geom_point() +
  labs(title="Lambda vs RMSE from Regularization Optimization") +
  theme_grey()

#append the results to the train, test, and validation results dataframes
train_results$y_hat_regularization<-unlist(regularization_results[3,which.min(regularization_results[2,])])
test_results$y_hat_regularization<-unlist(regularization_results[4,which.min(regularization_results[2,])])
validation_results$y_hat_regularization<-unlist(regularization_results[5,which.min(regularization_results[2,])])

#append RMSE results to table
train_rmse_table<-rbind(train_rmse_table,c("regularization",RMSE(train_results$y_hat_regularization,train_results$rating)))
test_rmse_table<-rbind(test_rmse_table,c("regularization",RMSE(test_results$y_hat_regularization,test_results$rating)))
validation_rmse_table<-rbind(validation_rmse_table,c("regularization",RMSE(validation_results$y_hat_regularization,validation_results$rating)))

#clean up the workspace 
rm(reg_effects, reg_effects_copy, lambda_tune, genre_effects, best_lambda, avg_ratings, user_avgs, lambdas, overall_mean)
gc()

#########################
#Use clustering to identify clusters of genres
#########################

set.seed(1)  

#create list of possible values of k, which is the number of clusters generated
k=seq(2,20,1)

#calculate the within-cluster sum of squares differences (ssd) for each possible k
ssd<-sapply(k,function(k_){
  print(paste("Testing with",k_,"clusters"))
  kms<-kmodes(genres_matrix[2:ncol(genres_matrix)], k_, iter.max = 1, weighted = FALSE, fast = T)
  #calculate and return the sum of squared errors among ovservations within each cluster
  sum(kms$withindiff)/nrow(genres_matrix)
})

#this plot shows us that the "elbow" occurs somewhere between 10-15, so 
ggplot(NULL, aes(x=k,y=ssd)) +
  geom_point() +
  theme_grey() +
  labs(x="Cluster Size k", y="Sum of With-Cluster Differences", title="Cluster Size K vs. Sum of Within-Cluster Differences")

#identify the best k (number of clusters) that minimizes differences among observations within clusters
best_k<-k[which(ssd==min(ssd))]

#using the optimal k to cluster the movies 
clusters<-kmodes(genres_matrix[2:ncol(genres_matrix)], best_k, iter.max = 5, weighted = FALSE, fast = T)

#print out the sizes of the clusters, in other words, the number of rows in each cluster
clusters$size

#create an object listing the cluster that each movie is assigned to it
genre_clusters<-cbind(genres_matrix[1],clusters$cluster)
names(genre_clusters)[2] <-"genre_cluster"

#remove any genres that fall into more than one cluster
genre_clusters<-subset(genre_clusters, !duplicated(subset(genre_clusters, select=c(title))))

#add the genre clusters to their respective rows in the train and test data
train <- left_join(train, genre_clusters, by="title")
test <- left_join(test, genre_clusters, by="title")
validation <- left_join(validation, genre_clusters, by="title")

#add columns for the user's average rating for each cluster
train <- train %>% group_by(genre_cluster, userId) %>%
  mutate(genre_cluster_user_avg=mean(rating))

#create a small df with just the userId, genre_cluster, and genre_cluster_user_avg
avgs <- train %>% ungroup() %>% select(c("userId","genre_cluster","genre_cluster_user_avg"))
avgs <- distinct(avgs)

#join the genres_cluster_user_avg average from the train dataset to the test dataset, using userId and genre_cluster as the join keys
test <- left_join(test,avgs,by=c("userId","genre_cluster"))
validation <- left_join(validation,avgs,by=c("userId","genre_cluster"))

#some users have not rated any movies in some clusters, so the genre_cluster_user_avg column has some NAs
train$genre_cluster_user_avg[is.na(train$genre_cluster_user_avg)] <- mean(train$rating)
validation$genre_cluster_user_avg[is.na(validation$genre_cluster_user_avg)] <- mean(train$rating)

#in both test and validation datasets, replace NAs with global average rating for the cluster
test$genre_cluster_user_avg[is.na(test$genre_cluster_user_avg)] <- mean(train$rating)
validation$user_avg[is.na(validation$user_avg)] <- mean(train$rating)

#clean up the work space by removing objects we don't need anymore
rm(avgs, clusters, genre_clusters, best_k, k, ssd)

#########################
#Model training
#########################

#create a truncated df that has just columns for the parameters used in the stepwise regression
train <- train %>% ungroup() %>% select(-c("userId", "movie_year", "rating_year", "title","movieId","genres","timestamp"))
test <- test %>% ungroup() %>% select(-c("userId","movie_year", "rating_year", "title","movieId","genres","timestamp"))
validation <- validation %>% ungroup() %>% select(-c("userId", "movie_year", "rating_year", "title","movieId","genres","timestamp"))

#take sample of observations from the train set
sample_shrinkage_factor <- 5
sample_index<-sample(seq(1,nrow(train),1),nrow(train)/sample_shrinkage_factor, replace = F)
train_results_sample<-train_results[sample_index,]

#now take sample for test set
test_sample_index<-sample(seq(1,nrow(test),1),nrow(test)/(sample_shrinkage_factor), replace = F)
test_results_sample<-test_results[test_sample_index,]

#estimate the best stepwise regression model based on 25 boostraps
best_stepwise_fit<-train(rating ~ ., method = "leapSeq", data = train[sample_index,],
                         tuneGrid=data.frame(nvmax=seq(1,ncol(train)-1,1)))

#summarize the predictors  included in the final model. (predictors included have an astrix)
summary(best_stepwise_fit$finalModel)

#the optimal number of predictors is:
best_stepwise_fit$bestTune

#append the predictions to the results datasets
train_results_sample[["y_hat_stepwise_regression"]]<-predict(best_stepwise_fit,train[sample_index,])
test_results_sample[["y_hat_stepwise_regression"]]<-predict(best_stepwise_fit,test[test_sample_index,]) 
validation_results[["y_hat_stepwise_regression"]]<-predict(best_stepwise_fit,validation) 

#update rmse tables
train_rmse_table<-rbind(train_rmse_table,c("stepwise_regression",min(best_stepwise_fit$results$RMSE)))
test_rmse_table<-rbind(test_rmse_table,c("stepwise_regression",RMSE(test_results_sample[["y_hat_stepwise_regression"]],test_results_sample$rating)))
validation_rmse_table<-rbind(validation_rmse_table,c("stepwise_regression",RMSE(validation_results[["y_hat_stepwise_regression"]],validation_results$rating)))

#########################
#Ensemble meta model 
#########################

#make meta model that predicts rating based on an average of 
#the predictions from the ensemble of models with 4 lowest RMSE
test_rmse_table[["rank"]]<-as.integer(rank(test_rmse_table$rmse))

#find the optimum number of models to include in the meta model
poss_num_models <- seq(2,nrow(test_rmse_table),1)
rmses<- sapply(poss_num_models,function(poss_models){
  models_to_keep <- test_rmse_table %>% filter(rank<poss_models)
  results_to_keep <- lapply(list(models_to_keep$model), function(k){
    paste("y_hat",k,sep="_")
  })
  
  y_hat_meta_model_test <- rowMeans(select(test_results_sample,c(unlist(results_to_keep))))
  RMSE(y_hat_meta_model_test,test_results_sample$rating)
})

which.min(rmses)
models_to_keep <- test_rmse_table %>% filter(rank<=which.min(rmses))
results_to_keep <- lapply(list(models_to_keep$model), function(k){
  paste("y_hat",k,sep="_")
})

print(paste("The models that were kept for the meta model include",models_to_keep[1]))

#append results of the meta model
train_results_sample[["y_hat_meta_model"]]<-rowMeans(select(train_results_sample,c(unlist(results_to_keep))))
test_results_sample[["y_hat_meta_model"]]<-rowMeans(select(test_results_sample,c(unlist(results_to_keep))))
validation_results[["y_hat_meta_model"]]<-rowMeans(select(validation_results,c(unlist(results_to_keep))))

#append RMSEs to RMSE tables
train_rmse_table<-rbind(train_rmse_table,c("meta_model",RMSE(train_results_sample$y_hat_meta_model,train_results_sample$rating)))
test_rmse_table<-rbind(test_rmse_table,c("meta_model",RMSE(test_results_sample$y_hat_meta_model,test_results_sample$rating)))
final_rmse_table<-test_rmse_table
validation_rmse_table<-rbind(validation_rmse_table,c("meta_model",RMSE(validation_results$y_hat_meta_model,validation_results$rating)))

#print the final RMSE table 
print(final_rmse_table)

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

