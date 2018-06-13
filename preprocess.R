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
#library("fastrtext")

stopwords <- scan("stopwords.txt", character(), quote = "")
stopwords <- c(stopwords, scan("customstopwords.txt", character(), quote = ""))
stopwords <- c(stopwords, as.character(0:10))

csvData <- fread("gender-classifier.csv", encoding = 'UTF-8')
csvData$tweet_id <- NULL
csvData$profileimage <- NULL
csvData$gender_gold <- NULL
csvData$profile_yn_gold <- NULL
csvData$'_unit_id' <- NULL
csvData <- csvData[csvData$profile_yn == "yes",]
csvData$profile_yn <- NULL
csvData <- csvData[csvData$gender != "unknown",]
csvData$gender <- as.factor(csvData$gender)
csvData$'_unit_state' <- NULL
csvData$diff_prof_twt <- as.numeric(difftime(mdy_hms(csvData$tweet_created), mdy_hms(csvData$created), units = c("weeks")))
csvData$tweets_per_day <- round(csvData$tweet_count / as.numeric(difftime(now("UTC"), mdy_hms(csvData$created), units = c("days"))))
csvData$tweet_created <- NULL
csvData$created <- NULL
csvData$`_last_judgment_at` <- NULL
csvData$name <- NULL
csvData$tweet_coord <- NULL
csvData$tweet_location <- NULL
csvData$`_golden` <- NULL
csvData$`_trusted_judgments` <- NULL
csvData$text <- str_replace_all(csvData$text, "&amp;", "&");


csvData[nchar(csvData$sidebar_color) != 6, "sidebar_color"] <- "ZZZZZZ" #seteo un color cuando no tienen un color valido
csvData$sidebar_color <- paste0("#", csvData$sidebar_color)

#https://i.imgur.com/PKjgfFXm.jpg
#mejorar
#hay mucho cyan por #C0DEED
#ver q onda los blancos , negros y que color setear cuando no tiene uno valido
hex2colorname <- function(hexacolor){
  if(hexacolor == "#ZZZZZZ")
    return("indeterminado")  
  
  coloresrgb <- col2rgb(hexacolor)[]
  hue <- rgb2hsv(coloresrgb[1,],coloresrgb[2,],coloresrgb[3,])["h",1] * 360
  
  if(hue >= 345 | hue < 30)
    return("red")
  if(hue >= 30 & hue < 45)
    return("orange")
  if(hue >= 45 & hue < 90)
    return("yellow")
  if(hue >= 90 & hue < 150)
    return("green")
  if(hue >= 150 & hue < 225)
    return("cyan")
  if(hue >= 225 & hue < 270)
    return("blue")
  if(hue >= 270 & hue < 285)
    return("violet")
  if(hue >= 285 & hue < 345)
    return("magenta")
}

csvData$sidebar_color_cat <- sapply(csvData$sidebar_color, hex2colorname)
ggplot(csvData, aes(sidebar_color_cat)) +
  geom_bar(aes(fill = gender), position=position_dodge())




armarVocabularios <- function (genero){
  tokens <- tokenize_tweets(csvData[csvData$gender == genero,]$text, stopwords = stopwords)
  tokens <- unlist(tokens)
  tokens <- tokens[is.na(as.numeric(tokens))] #filtro los numeros
  tokens <- tokens[!grepl("www.|http:|https:", tokens)] #filtro urls
  tokens <- tokens[str_length(tokens) > 2] #filtro palabras menores a 3
  
  dftokens <- count(tokens)
  names(dftokens) <- c("word", "freq")
  dftokensMost <- head(dftokens[order(-dftokens$freq),], 25)
  dftokensMost$relative <- dftokensMost$freq / sum(dftokensMost$freq) * 100
  dftokensMost$freq <- NULL
  
  return(dftokensMost)
}

vocabularios <- lapply(unique(csvData$gender), armarVocabularios)
vocabularios.male <- vocabularios[[1]]
vocabularios.female <- vocabularios[[2]]
vocabularios.brand <- vocabularios[[3]]

#tmp <- csvData[order(csvData$gender,csvData$diff_prof_twt),]
tmp <- csvData[order(csvData$gender),]
ggplot(data=tmp,aes(x=seq_along(tmp$diff_prof_twt), y=diff_prof_twt, color=gender)) +
  geom_point( )


tmp2 <- csvData[order(csvData$gender, csvData),]
ggplot(data=tmp2,aes(x=seq_along(tmp2$tweets_per_day), y=tweets_per_day, color=gender)) +
  geom_point( )




calculateVocabularyScore <- function(tweet_tokens, vocabulary){
  return(sum(vocabulary[match(tweet_tokens, vocabulary$word), "relative"], na.rm = TRUE))
}
tweets_tokens <- tokenize_tweets(csvData$text, stopwords = stopwords)
csvData$score_vocab_brand <- sapply(tweets_tokens, calculateVocabularyScore, vocabulary = vocabularios.brand)
csvData$score_vocab_male <- sapply(tweets_tokens, calculateVocabularyScore, vocabulary = vocabularios.male)
csvData$score_vocab_female <- sapply(tweets_tokens, calculateVocabularyScore, vocabulary = vocabularios.female)

predictOnScore <- function(rowSample){
  if(rowSample$score_vocab_brand > rowSample$score_vocab_male & rowSample$score_vocab_brand > rowSample$score_vocab_female)
    return("brand")
  if(rowSample$score_vocab_male > rowSample$score_vocab_brand & rowSample$score_vocab_male > rowSample$score_vocab_female)
    return("male")
  if(rowSample$score_vocab_female > rowSample$score_vocab_brand & rowSample$score_vocab_female > rowSample$score_vocab_male)
    return("female")
  
  return(NA)
}
rapply(csvData, predictOnScore)

match(tokenstext[[]], vocabularios.male)

match(tokenize_tweets(csvData$text, stopwords = stopwords)[[1]], vocabularios.female)
match(tokenize_tweets(csvData$text, stopwords = stopwords)[[1]], vocabularios.brand)

tokens <- tokenize_tweets(csvData$text, stopwords = stopwords)
tokens[[1000]]

match(tokens[[1000]], vocabularios.male$x)

  
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



