library(twitteR)
setup_twitter_oauth("cDJ9eqhEXlUPWeY7bV2TbmRNF","FYKaGkwJJx41AM6I4QFn8jBceSzSuQtNaqdMuRAYOB5XluefUY","3607051577-ndbDsuzThV5SYzFUrGQeGcttIDxKXd8RmWOxYY3","UotKyPNTDAcL4etQ9gBs5SK31CDoiFKufxMwY2GXUMagR")


drugs<-c("humira", "dronedarone", "lamictal", "pradaxa", "paxil","zoledronic acid","trazodone","enbrel"," cymbalta","quetiapine","cipro",
         "lozenge","dabigatran","olanzapine","fluoxetine","vyvanse","seroquel","fosamax","paroxetine"," nicotine","effexor","prozac",
         "tysabri","rivaroxaban","baclofen","lamotrigine","venlafaxine","apixaban","avelox","levaquin","zyprexa","duloxetine","ofloxacin",
         "geodon","victoza","metoprolol","viibryd","pristiq","nesina","factive","gamma-aminobutyric acid","sabril","livalo","denosumab",
         "bystolic","xarelto","floxin","boniva","saphris","ziprasidone","memantine","namenda","latuda","fycompa","canagliflozin","zometa",
         "etanercept","lurasidone","alendronate","linagliptin","effient","vimpat","eliquis","liraglutide","pregabalin","onglyza","nicotrol inhaler",
         "lyrica","invokana","commitlozenge","actonel","nicotrolinhaler","synthroid","albuterol","nasonex","spiriva","suboxone","nexium",
         "januvia","valsartan","tamiflu")
tweets = list()
for(i in 1:length(drugs)){
  result<-searchTwitter(drugs[i],n=1500, since="2016-01-01")
  tweets <- c(tweets,result)
  tweets <- unique(tweets)
}
         










