#' Clean tweets
#'
#' @description Does the data cleaning as descirbed in the markdwown
#'
#' @param train.data data frame
#' @param drug.list data frame
#' @slang.lookup data frame
#'
#' @return data frame

clean_tweets <- function(train.data, drug.list, slang.lookup) {
    
    #Loading the emoticon dataset from qdapDictionaries. This converts emoticons to their english text counterpart
    data("emoticon")
    
    train.data.new <- train.data %>%
        mutate(
            #ae_term       = substr(tweet_text,start_offset,end_offset),      # Extract ae terms (only applicable here)
            hashtag       = str_extract_all(tweet_text,"#[a-zA-Z]+"),        # Collect all hashtags
            URL           = grepl("http[[:alnum:][:punct:]]*", tweet_text)   # Boolean to indicate if tweet contains UR
            
        ) %>%
        arrange(tweet_id) %>%
        mutate(rown = row_number())
    
    
    
    
    # Reshaping the data
    # Transposing to get one word per row using the strsplitstack package. This allows us to preserve emoticons
    train.data.long <- train.data.new %>%
        select(tweet_id, tweet_text) %>%
        mutate(words = strsplit(as.character(tolower(tweet_text))," ")) %>%
        unnest() %>%
        group_by(tweet_id) %>%
        mutate(wordn = row_number()) %>%
        ungroup %>%
        left_join(emoticon, by=c("words"="emoticon"))             # Add the meaning for the emoticons
    
    
    train.data.aug <- train.data.long %>%
        mutate(
            is_hashtag = grepl("#[a-zA-Z]+",words),                 # attribute to show if its a hashtag
            is_URL     = grepl("http[[:alnum:][:punct:]]*",words),  # is it a url
            words      = gsub("([:lower:])\\1+","\\1\\1", words),   # Contract word length
            words      = gsub("[[:punct:]]", "", words)             # Remove punctuation
        ) %>%
        filter(words != "") %>%
        left_join(mutate(drug.list, drug_name=str), by=c("words"="str")) %>%
        left_join(slang.lookup, by="words")
    
    
    
    # this part runs really slow, alex, can you please take a look at it
    train.data.clean <- train.data.aug %>%
        rowwise() %>%
        mutate(
            wordsx = ifelse((is.na(drug_name) && !is_hashtag && !is_URL),
                            ifelse (hunspell_check(words),
                                    words,
                                    ifelse(is.na(decode),
                                           words,#hunspell_suggest(word)[[1]][1]
                                           decode 
                                    )
                            ),
                            words
            )
        )
    
    # aggregating using the aug dataset, once the previous step is made to work faster, this will need to read from the cleaned dataframe
    train.data.wide <- train.data.aug %>%
        group_by(tweet_id) %>%
        summarise(cleaned_tweet = paste(words, collapse = " "))
    
    
    
    # Create corpus
    tweet.corpus <- Corpus(VectorSource(train.data.wide$cleaned_tweet))
    
    
    tweet.corpus <- tm_map(tweet.corpus, stemDocument)
    tdm <- TermDocumentMatrix(tweet.corpus,
                              control = list(
                                  removePunctuation = TRUE,
                                  stopwords = TRUE))
    require("tm.lexicon.GeneralInquirer")
    
    pos.score <- tm_term_score(tdm, terms_in_General_Inquirer_categories("Positiv")) # this lists each document with number below
    
    neg.score <- tm_term_score(tdm,terms_in_General_Inquirer_categories("Negativ"))
    
    
    cleaned_tweets <- data.frame(stemmed = sapply(tweet.corpus, as.character),
                                 positive = pos.score,
                                 negative = neg.score,
                                 stringsAsFactors = FALSE)
    
    
    
    cleaned_tweets <- cleaned_tweets %>%
        mutate(rown = row_number(),
               positivity = (positive - negative) / (positive + negative + 1))
    
    
    cleaned_tweets <-
        cleaned_tweets %>%
        inner_join(train.data.new, by="rown")
    
    #Remove drugs from text
    drugs <-
        read_lines("../data/download_tweets/drug_names.txt", skip = 6) %>% 
        stemDocument()
    drug_regex <- drugs %>% str_c(collapse = "|") %>% str_c("(", ., ")")
    
    # Remove hashtag symbol
    cleaned_tweets$hashtag %<>% str_replace_all("#", "")
    
    # Change hashtags
    cleaned_tweets$hashtag %<>% str_to_lower() %>% stemDocument()
    cleaned_tweets$hashtag <- plyr::revalue(cleaned_tweets$hashtag, c("character(0)" = NA))
    
    # Extract drug
    cleaned_tweets$drug <- 
        str_c(str_extract_all(cleaned_tweets$stemmed, drug_regex),
              str_extract_all(cleaned_tweets$hashtag, drug_regex)) %>% 
        str_replace_all("character\\(0\\)", "")
    
    # Remove drug from tweet and hashtag
    cleaned_tweets$stemmed %<>% str_to_lower %>%  str_replace_all(drug_regex, "")
    
    cleaned_tweets$hashtag %<>% 
        str_replace_all(drug_regex, "") %>% 
        str_replace_all('", ', "") %>% 
        str_replace_all('c\\("', "") %>% str_replace_all('"\\)', "") %>%
        str_replace_all('\\"', " ") %>% 
        ifelse(. == "", NA, .)
    
    cleaned_tweets
}