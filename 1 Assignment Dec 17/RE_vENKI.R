### Required packages for this project
library(dplyr)
library(tidyr)
library(ggplot2)
library(arules)
library(caret)
library(animation)
library(lsa) #Latent semantic analysis (for cosine function)
#__________________________

ds <- read.csv(file.choose())
str(ds)
ds1 <- ds #Keep the backup
ds <- ds[-4] #Drop the 4th column as we don't require this
#__________________________

###User-Based Collaborative Filtering

ubcfm <- spread(ds, key = movieId, value = rating) #Arranging rows as users, columns as movies and ratings are cell values
str(ubcfm)
rownames(ubcfm) <- ubcfm[,1] #Changing Row names with user id's
ubcfm <- ubcfm[-1] #We have transformed row names so the first column is no longer needed
rmeans <- rowMeans(ubcfm,na.rm = TRUE) #To calculate centered cosine, calculate row means
uccs <- sweep(ubcfm, 1,rmeans,"-") #Created centered cosine by substracting every row value with respective row mean
uccs[is.na(uccs)] <- 0 #Transform NA's as 0
muccs <- t(as.matrix(uccs)) #Transforming data frame as matrix, cosine will accept vectors or matrix

sim <- cosine(muccs) #Calculating similarity matrix
sim <- round(sim,3) #Similarity values are having longer decimal numbers, so rouding to 3 decimal point

#__________________________

recommend_movies <- function(user){
  
  sim_users <- as.integer(names(sim[`user`,sim[`user`,] > 0]))[-1]
  user_not_rated_movies <- as.integer(names(ubcfm[`user`,is.na(ubcfm[`user`,])]))
  user_not_rated_movies <- user_not_rated_movies[1:50]
  wr <- vector()
  svc <- vector()
  movie <- vector()
  rating <- vector()
 
   for(i in user_not_rated_movies){
    for(j in sim_users){
      if(!is.na(ubcfm[`j`,`i`]))
        wr <- append(wr, sim[`user`,j]*ubcfm[`j`,`i`], after = length(wr))
        svc <- append(svc, sim[`user`,j], after = length(svc))
      
    }
    movie <- append(movie, i, after = length(movie))
    rating <- append(rating, sum(wr)/sum(svc), after = length(rating))
    
   }
  result <- data.frame(movie,rating)
  result <- result %>%
              arrange(desc(rating))
  return(result)
}

#__________________________


recommend_movies(600)





