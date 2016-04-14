#' Parse downloaded data
#'
#' @description Parses the data from the DIEGO labs. Depending on the argument 
#' it reads in the training or test data. It joins the tweets with the 
#' annotations. Empty annotations means no adverse reaction.
#' 
#' @author AlexandeR Noll
#'
#' @param type = c("train", "test")
#'
#' @return data frame
#'
#' @examples parse_diego_data("train")
#' @import dplyr, readr, stringr


parse_diego_data <- function(type) {
    library(dplyr)
    library(readr)
    library(stringr)
    
    # Load data
    annotation_file <- ifelse(type == "train", 
                              "../data/download_tweets/train_tweet_annotations.tsv",
                       ifelse(type == "test",
                              "../data/download_tweets/test_tweet_annotations.tsv",
                              stop("type should be either 'train' or 'test'"
                                    ))
                       )
    
    tweets_file <- ifelse(type == "train",
                          "../data/download_tweets/full_train_tweet_ids.tsv",
                   ifelse(type == "test",
                          "../data/download_tweets/full_test_tweet_ids.tsv",
                          stop("type should be either 'train' or 'test'"))
                   )
    
    
    # Read annotations and rename columns
    annotations <- 
        read_tsv(annotation_file,
                 col_names = FALSE,
                 col_types = c("ciicccc")
        ) %>% 
        rename(text_id = X1,
               start_offset = X2,
               end_offset = X3,
               semantic_type = X4,
               annotated_text = X5,
               related_drug = X6,
               target_drug = X7
        )
    
    # Parse tweets
    tweets <- 
        read_lines(tweets_file) %>% 
        str_split(., "\t") %>% 
        as.data.frame() %>% 
        as.matrix() %>% 
        t %>% 
        as.data.frame() %>% 
        `rownames<-`(., NULL) %>% 
        rename(tweet_id = V1,
               user_id = V2,
               text_id = V3,
               tweet_text = V4
        )
    
    # Join the two together
    result <- 
        suppressWarnings(left_join(tweets, annotations, by = "text_id"))
    
    result
    
}