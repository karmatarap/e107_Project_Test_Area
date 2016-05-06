#' Makes drug list
#'
#' @description Creates drug list as described in data cleaning markdown
#'
#' @return data frame
#'
#' @examples make_drug_list()


make_drug_list <- function() {
    # Read drug names
    rx.conso <- read.delim(file = '../data/dictionaries/RXNCONSO.RRF',
                           sep='|',
                           header = F,
                           stringsAsFactors = F)
    
    # Column names
    rx.colstr <- "RXCUI LAT TS LUI STT SUI SPREF RXAUI SAUI SCUI SDUI SAB TTY CODE STR SRL SUPPRESS CVF"
    
    # Assign column names to data frame
    names(rx.conso) <- rx.colstr %>%
        tolower %>%
        strsplit(split=' ') %>%
        unlist
    
    # Subset a list of drug brand names (BN) and generics/ingredient name (IN)
    # convert to lower for easier lookup
    drug.list <- rx.conso %>%
        subset((tty %in% c('BN','IN'))) %>%
        select(tty, code, str) %>%
        mutate(str = tolower(str))
    
    drug.list

    }