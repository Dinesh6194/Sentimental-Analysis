library(dplyr)
library(tidytext)
library(qdapTools)
library(qdapRegex)
library(qdapDictionaries)
library(textstem)
library(textclean)
library(textcat)
library(quanteda)

####################################
mydata <- read.csv(file="train_data.csv", header=TRUE, sep=",")

mydata1 <- head(mydata,6500)

###########Data Wrangling

mydata1[[2]] <- tolower(mydata1[[2]])
mydata1[[2]] <- rm_url(mydata1[[2]],pattern = pastex("@rm_twitter_url","@rm_url"))
mydata1[[2]] <- gsub('#\\S+', '', mydata1[[2]])
mydata1[[2]] <- gsub('@\\S+', '', mydata1[[2]])
mydata1[[2]] <- replace_contraction(mydata1[[2]])

mydata1[[2]] <- removePunctuation(mydata1[[2]],preserve_intra_word_dashes = TRUE)
mydata1[[2]] <- removeNumbers(mydata1[[2]])

mydata1[[2]] <- rm_nchar_words(mydata1[[2]],"1,2",replacement = "")

mydata2 <- unnest_tokens(mydata1,tokenised_col,text,token = "words")
mydata2[[2]] <- lemmatize_words(mydata2[[2]])

mydata3 <- mydata2 %>%
  anti_join(stop_words, by= c("tokenised_col" = "word"))

##mydata3$postag <- pos(mydata3[[2]])

mydata4 <- mydata3 %>% group_by(trn_id) %>% 
  summarise(val=paste(tokenised_col, collapse=","))

mydatadum
mydata3
###########################



#remove 95% of words 
# remove words not in english language

colnames(mydata3)
head(mydata4,173)

mydata4[173,]

mydata1[173,]
mydata1[20,]

