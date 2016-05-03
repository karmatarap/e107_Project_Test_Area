#' Predict new tweet
#'
#' @description This function predicts the score for a new tweet based on the
#' model trained
#'
#' @return numeric
#'
#' @examples predict_new_tweet("I feel sick today")

predict_new_tweet <- function(new_tweet) {
    library(plyr)
    
    library(caret)
    library(dplyr)
    library(hash)
    library(hunspell)
    library(magrittr)
    library(qdap)
    library(qdapDictionaries)
    library(readr)
    library(RTextTools)
    library(rvest)
    library(stringr)
    library(tidyr)
    library(tm)
    library(tm.lexicon.GeneralInquirer)
    library(tm.plugin.sentiment)
    
    new_tweet <- rep(new_tweet, 5)
    
    source("./clean_tweets.R")
    source("./make_slang_lookup.R")
    source("./make_drug_list.R")
    
    load("../data/models/fit_models.RData")
    load("../data/models/stacked_rf.RData")
    load("../data/models/doc_matrix.RData")
    load("../data/models/train_data.RData")
    
    cleaned_tweet <-
        clean_tweets(data.frame(tweet_id = 1:length(new_tweet), tweet_text = new_tweet),
                     make_drug_list(),
                     make_slang_lookup())
    
    
    source("../r/helper_functions/create_matrix_fixed.R")
    new_tweets_dtm <- 
        create_matrix(cleaned_tweet$stemmed,
                      originalMatrix = doc_matrix,
                      language = "english",
                      removeNumbers = TRUE,
                      removePunctuation = TRUE,
                      toLower = TRUE,
                      stemWords = TRUE,
                      stripWhitespace = TRUE,
                      ngramLength = 1,
                      minDocFreq = 1,
                      weighting = tm::weightTf)
    
    new_container <- create_container(new_tweets_dtm,
                                      labels = c(data[[1]]$is_AE, data[[2]]$is_AE),
                                      trainSize = NULL,
                                      testSize = 1:nrow(cleaned_tweet),
                                      virgin = TRUE
    )
    
    preds_new <- classify_models(new_container, fit_models)
    preds_new_final <- predict(fit_rf, preds_new, type = "prob")[, 2]
    
    preds_new_final[1]
}