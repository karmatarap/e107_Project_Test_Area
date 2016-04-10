###########################
# Setup -------------------------------------------------------------------
###########################
library(dplyr)
library(purrr)
library(stringr)
library(twitteR)

setup_twitter_oauth(consumer_key = "xxx", 
                    consumer_secret = "xxx", 
                    access_token = "xxx", 
                    access_secret = "xxx"
                    )

test_data <-
    searchTwitter('humira',
                  n = 50,
                  since = "2015-01-01"
    ) %>% 
    c(searchTwitter('enbrel', n = 5000))

save(test_data, file = "./test_data.RData")

map_chr(test_data, ~ as.character(.$text)) %>% 
    str_sub(1, 10) %>% 
    min
    View
