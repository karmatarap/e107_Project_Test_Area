#' Parse binary data
#'
#' @description Parses the data with binary annotation¨
#' 
#' @author AlexandeR Noll
#'
#' @return data frame
#'
#' @examples parse_binary_data()
#' @import dplyr, readr, stringr


parse_binary_data <- function(type) {
    library(dplyr)
    library(purrr)
    library(readr)
    library(stringr)
    
    # Load data
    tweets_file <- "../data/download_tweets/binary_tweets_downloaded.tsv"
    
    # Parse tweets
    individual_tweets <- 
        read_file(tweets_file) %>% 
        str_replace_all("\r", "") %>% 
        str_split(., "\n") %>%
        unlist()
    
    tweets_frame <- 
        individual_tweets %>% 
        str_split_fixed("\t", 4) %>% 
        as.data.frame() %>% 
        rename(tweet_id = V1,
               user_id = V2,
               is_AE = V3,
               tweet_text = V4
        ) %>% 
        filter(is_AE != "") %>% 
        mutate(is_AE = as.factor(plyr::revalue(is_AE, c("0" = "No", "1" = "Yes"))))
    
    tweets_frame
}