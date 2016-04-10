
library(twitteR)

#setup these from twitter dev page
apiKey <-  "vH66cVfWfr5npeU77hqqI7Wdl"
apiSecret <- "hzf1fUIdCAM0Fh86BbcfKm7Sdc4Qlw7bmbqoMW6jJCWKm46W6m"
accessToken <- "716327334828195840-74IcWZjju7ZAvlQP4S1UOodJsHJ4sSN"
accessTokenSecret <- "x1o88za3OZdjeiJGeKfpGbQ3n1C5aYiaspth9Trzq66iP"


setup_twitter_oauth(apiKey, 
                    apiSecret, 
                    accessToken, 
                    accessTokenSecret)


# harvest some tweets
tweets <- searchTwitter("enbrel",n=60)
tweets
