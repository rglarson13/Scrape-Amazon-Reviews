library(tidyverse)

dataset_original <- read.delim('Amazon Reviews B002CJN0H2 2018-09-27.tsv', 
                               sep = '\t', 
                               quote = '',
                               stringsAsFactors = FALSE
                               )


# Clean up the rating and date fields
library(stringr)

dataset_original <- dataset_original %>%
  mutate(reviewRating = as.numeric(str_sub(reviewRating, start = 1, end = 1))) %>%
  mutate(reviewDate = as.Date(str_sub(reviewDate, start = 4, end = -1), '%B %d, %Y'))

# If you want to filter to just 1/5 star, or something
#dataset_original <- dataset_original %>% filter(reviewRating == 5 | reviewRating == 1)


# Clean the text
library(tm)
corpus <- VCorpus(VectorSource(dataset_original$reviewText))

corpus <- tm_map(corpus, content_transformer(tolower)) # content transformation (lowercase-ifier) mapped to the text transfomer
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, removePunctuation)

# remove stopwords
library(SnowballC)
corpus <- tm_map(corpus, removeWords, stopwords())

# apply stemming and strip whitespace
corpus <- tm_map(corpus, stemDocument)
corpus <- tm_map(corpus, stripWhitespace) # I think the stemDocument transform does this, too...?

# Creating the Bag of Words model
dtm <- DocumentTermMatrix(corpus)
dtm <- removeSparseTerms(dtm, 0.99) 


# Build the classification model

dataset <- as.data.frame(as.matrix(dtm)) # convert the matrix back to a dataframe
feat_cols <- ncol(dataset)
dataset$reviewRating <- dataset_original$reviewRating

# Encode the star label as a factor
dataset$reviewRating <- factor(dataset$reviewRating, levels = c(1:5), ordered = FALSE)

library(caTools)
set.seed(123)

split <- sample.split(dataset$reviewRating, SplitRatio = 0.80)
training_set <- subset(dataset, split == TRUE)
test_set <- subset(dataset, split == FALSE)

# quasi-histogram of the distribution of review ratings
ggplot(data = test_set, aes(x = reviewRating)) + geom_bar()



########################### TRAIN CLASSFIERS ###########################

# Fit the Random Forest Classifier to the Training Set
library(randomForest)
classifier <- randomForest(x = training_set[1:feat_cols],
                           y = training_set$reviewRating,
                           ntree = 100)

# Naive Bayes
library(e1071)
nb_classifier <- naiveBayes(x = training_set[1:feat_cols],
                            y = training_set$reviewRating)

# RPaRT + Bagging
library(rpart)
library(adabag)
bag_classifier <- bagging(reviewRating ~ ., 
                          data = training_set, 
                          mfinal = 100, 
                          control = rpart.control(minsplit = 2))





# Predict the Test Set results
y_pred <- predict(classifier, newdata = test_set[1:feat_cols])
y_pred_nb <- predict(nb_classifier, newdata = test_set[1:feat_cols])
y_pred_bag <- predict(bag_classifier, newdata = test_set[1:feat_cols], newmfinal = 100)

# Create the confusion matrices
cm <- table(test_set[, (feat_cols + 1)], y_pred)
cm_nb <- table(test_set[, (feat_cols + 1)], y_pred_nb)
cm_bag <- table(test_set[, (feat_cols + 1)], y_pred_bag)

# Calculate accuracy scores
accuracy <- ((cm[[1,1]] + cm[[2,2]] + cm[[3,3]] + cm[[4,4]] + cm[[5,5]]) / nrow(test_set))
accuracy_nb <- ((cm_nb[[1,1]] + cm_nb[[2,2]] + cm_nb[[3,3]] + cm_nb[[4,4]] + cm_nb[[5,5]]) / nrow(test_set))
accuracy_bag <- ((cm_bag[[1,1]] + cm_bag[[2,2]] + cm_bag[[3,3]] + cm_bag[[4,4]] + cm_bag[[5,5]]) / nrow(test_set))


# Print 'em out
print('Random Forest')
print(cm)
print(accuracy)

print('Naive Bayes')
print(cm_nb)
print(accuracy_nb)

print('Bagging')
print(cm_bag)
print(accuracy_bag)