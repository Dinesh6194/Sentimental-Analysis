library(tm)
library(dplyr)

reviews <- read.csv("train_data.csv", stringsAsFactors = F)
labels <- read.csv("train_label.csv", stringsAsFactors = F)
merged_label <-  merge(reviews, labels, by.x = "trn_id", by.y = "trn_id")

test_reviews <- merged_label[1:350000, ]

removeURL <- content_transformer(function(x) gsub("(f|ht)tp(s?)://\\S+", "", x, perl=T))

review_corpus <- VCorpus(VectorSource(test_reviews$text))
review_corpus = tm_map(review_corpus, removeURL)
review_corpus = tm_map(review_corpus, content_transformer(tolower))
review_corpus = tm_map(review_corpus, removeNumbers)
review_corpus = tm_map(review_corpus, removePunctuation)
review_corpus = tm_map(review_corpus, removeWords, c("the", "and", stopwords("english")))
review_corpus = tm_map(review_corpus, stemDocument, language = "english")
review_corpus =  tm_map(review_corpus, stripWhitespace)

inspect(review_corpus[1])

review_dtm_tfidf <- DocumentTermMatrix(review_corpus, control = list(weighting = weightTfIdf))
review_dtm_tfidf = removeSparseTerms(review_dtm_tfidf, 0.99)
review_dtm_tfidf
inspect(review_dtm_tfidf[1,1:20])



test_reviews$text = NULL
test_reviews = cbind(test_reviews, as.matrix(review_dtm_tfidf))
test_reviews$label =  as.factor(test_reviews$label)
test_reviews$trn_id = NULL

id_train <- sample(nrow(test_reviews),nrow(test_reviews)*0.80)
test_reviews.train = test_reviews[id_train,]
test_reviews.test = test_reviews[-id_train,]

# test_reviews.train$label2 <- relevel(test_reviews.train$label, ref = "1")
library(nnet)
model <- nnet::multinom(label ~., data = test_reviews.train, MaxNWts = 44900)
predicted.classes <- model %>% predict(test_reviews.test)

table(test_reviews.test$label,predicted.classes,dnn=c("Obs","Pred"))
