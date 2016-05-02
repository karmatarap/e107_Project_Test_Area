#' Predict new tweet
#'
#' @description This function predicts the score for a new tweet based on the
#' model trained
#'
#' @return numeric
#'
#' @examples predict_new_tweet("I feel sick today")



predict_new_tweet <- function(new_tweet, model) {
    
    new_tweet <- rep(new_tweet, 5)
    
    cleaned_tweet <-
        clean_tweets(data.frame(tweet_id = 1:length(new_tweet), tweet_text = new_tweet),
                     make_drug_list(),
                     make_slang_lookup())
    
    #Remove drugs from text
    drugs <-
        read_lines("../data/download_tweets/drug_names.txt", skip = 6) %>% 
        stemDocument()
    drug_regex <- drugs %>% str_c(collapse = "|") %>% str_c("(", ., ")")
    
    # Remove hashtag symbol
    cleaned_tweet$hashtag %<>% str_replace_all("#", "")
    
    # Change hashtags
    cleaned_tweet$hashtag %<>% str_to_lower() %>% stemDocument()
    cleaned_tweet$hashtag <- plyr::revalue(cleaned_tweet$hashtag, c("character(0)" = NA))
    
    # Extract drug
    cleaned_tweet$drug <- 
        str_c(str_extract_all(cleaned_tweet$stemmed, drug_regex),
              str_extract_all(cleaned_tweet$hashtag, drug_regex)) %>% 
        str_replace_all("character\\(0\\)", "")
    
    # Remove drug from tweet and hashtag
    cleaned_tweet$stemmed %<>% str_to_lower %>%  str_replace_all(drug_regex, "")
    
    cleaned_tweet$hashtag %<>% 
        str_replace_all(drug_regex, "") %>% 
        str_replace_all('", ', "") %>% 
        str_replace_all('c\\("', "") %>% str_replace_all('"\\)', "") %>%
        str_replace_all('\\"', " ") %>% 
        ifelse(. == "", NA, .)
    
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