library(tm)
library(dplyr)
library(tidytext)
library(tidyr)

reviews <- read.csv("train_data.csv", stringsAsFactors = F)
labels <- read.csv("train_label.csv", stringsAsFactors = F)
merged_label <-  merge(reviews, labels, by.x = "trn_id", by.y = "trn_id")

test_reviews <- merged_label[1:100, ]

test_reviews <- mutate(test_reviews, 
                       text = gsub(x = text, pattern = "[0-9]+|[[:punct:]]|\\(.*\\)", replacement = ""))

unnest_tokens(test_reviews,bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  filter(!word1 %in% stop_words$word,
         !word2 %in% stop_words$word) %>%
  unite(bigram, word1, word2, sep = '_')  %>% 
  group_by(trn_id,label)  %>% summarise(bigram=paste(bigram, collapse=" ")) -> test_reviews

review_corpus <- VCorpus(VectorSource(test_reviews$bigram))
# review_corpus = tm_map(review_corpus, removeURL)
# review_corpus = tm_map(review_corpus, content_transformer(tolower))
# review_corpus = tm_map(review_corpus, removeNumbers)
# review_corpus = tm_map(review_corpus, removePunctuation)
# review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
# review_corpus = tm_map(review_corpus, stemDocument, language = "english")
# review_corpus =  tm_map(review_corpus, stripWhitespace)

inspect(review_corpus[1])

review_dtm_tfidf <- DocumentTermMatrix(review_corpus ,control = list(weighting = weightTfIdf))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.99)
review_dtm_tfidf
# inspect(review_dtm_tfidf[2,110:130])

test_reviews$bigram = NULL
test_reviews = cbind.data.frame(test_reviews,as.matrix(review_dtm_tfidf))
test_reviews$trn_id = NULL
test_reviews$label = as.factor(test_reviews$label)
test_reviews = da(test_reviews)
typeof(test_reviews)

id_train <- sample(nrow(test_reviews),nrow(test_reviews)*0.80)
test_reviews.train = test_reviews[id_train,]
test_reviews.test = test_reviews[-id_train,]

library(caret)
svm_model <- train(label~.,data = test_reviews.train, method = 'svmLinear3')
output <- predict(svm_model, new =test_reviews.test)

# test_reviews.svm = svm(label~., data = test_reviews.train)
# pred.svm = predict(test_reviews.svm, test_reviews.test)

table(test_reviews.test$label,output,dnn=c("Obs","Pred"))
