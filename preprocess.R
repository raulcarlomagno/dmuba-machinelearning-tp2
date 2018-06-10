library("data.table")
library("DataExplorer")
library("ggplot2") 
library("lubridate")
#library("arules") #detach("package:arules", unload = TRUE)
library("tokenizers")
#library("stopwords") #detach("package:stopwords", unload = TRUE)
#library("tm") #detach("package:tm", unload = TRUE)
library(plyr)
library("stringr")

stopwords <- scan("stopwords.txt", character(), quote = "")
stopwords <- c(stopwords, scan("customstopwords.txt", character(), quote = ""))
stopwords <- c(stopwords, as.character(0:10))

csvData <- fread("gender-classifier.csv")
csvData$tweet_id <- NULL
csvData$profileimage <- NULL
csvData$gender_gold <- NULL
csvData$profile_yn_gold <- NULL
csvData$'_unit_id' <- NULL
csvData <- csvData[csvData$profile_yn == "yes",]
csvData$profile_yn <- NULL
csvData <- csvData[csvData$gender != "unknown",]
csvData$'_unit_state' <- NULL
csvData$diff_prof_twt <- as.numeric(difftime(mdy_hms(csvData$tweet_created), mdy_hms(csvData$created), units = c("weeks")))
csvData$tweet_created <- NULL
csvData$created <- NULL
csvData$`_last_judgment_at` <- NULL
csvData$name <- NULL
csvData$`_golden` <- NULL
csvData$`_trusted_judgments` <- NULL
csvData$text <- str_replace_all(csvData$text, "&amp;", "&");

# ver lo de los colores
# colores <- paste0("#",subset(csvData$sidebar_color, nchar(csvData$sidebar_color) == 6  )  )
# coloresrgb <- col2rgb(colores)[]
# coloresint <- coloresrgb[1,] * 256 * 256 + coloresrgb[2,] * 256 + coloresrgb[3,]
# 256*256*red+256*green+blue
# qplot(coloresint)
# qplot( discretize(coloresint) )


qplot(tokenize_word_stems(csvData$text, stopwords = stopwords::stopwords("en")))

unique(tokenize_words(csvData$text, stopwords = stopwords::stopwords("en")))



procesarPalabras <- function (genero){
  tokens <- tokenize_tweets(csvData[csvData$gender == genero,]$text, stopwords = stopwords)
  tokens <- unlist(tokens)
  tokens <- tokens[is.na(as.numeric(tokens))] #filtro los numeros
  tokens <- tokens[str_length(tokens) > 2]
    
  dftokens <- count(tokens)
  return(head(dftokens[order(-dftokens$freq),], 30)[,1])
}

listas <- sapply(unique(csvData$gender), procesarPalabras)
listas



str(csvData)

plot_missing(csvData)
plot_histogram(csvData)
plot_bar(csvData)
plot_boxplot(csvData, by = "gender")
create_report(csvData)

summary(csvData$profile_yn)

unique(csvData$gender_gold)
qplot(csvData$gender_gold)

table(csvData$profile_yn_gold)
table(csvData$profile_yn_gold) / sum(table(csvData$profile_yn_gold))



