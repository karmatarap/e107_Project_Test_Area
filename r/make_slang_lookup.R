#' Makes slang lookup dictionary
#'
#' @description Creates slang lookup dictionary as described in data cleaning
#' markdown
#'
#' @return data frame
#'
#' @examples make_slang_lookup()
#' 
make_slang_lookup <- function() {
    url <- "http://www.netlingo.com/acronyms.php"
    
    contents <- read_html(url)
    
    #write(contents, file = "../data/dictionaries/slang.txt")
    
    #contents <- read.table("../data/dictionaries/slang.txt")
    
    #lapply(contents, write, "../data/dictionaries/slang_words", append=TRUE, ncolumns=1000)
    
    # Using the chrome selector gadget we identified the following attributes containing the dictionary: .list_box3 li, .list_box3 span
    words <- contents %>%
        html_nodes(".list_box3 span") %>%
        html_text() %>% tolower
    
    descriptions <- contents %>%
        html_nodes(".list_box3 li") %>%
        html_text() %>% tolower
    
    slang.lookup <- data.frame(words, descriptions, stringsAsFactors = FALSE)
    
    # Cleaning out the descriptions, as it contains the slang words also
    # Also removing slangs that can have multiple interpretations
    # Remove the null key
    slang.lookup <- slang.lookup %>%
        mutate(decode = substring(descriptions,nchar(words)+1)) %>%
        filter(!grepl(",|-or-",descriptions)) %>%
        filter(words != "")
}