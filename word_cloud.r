library(tidyverse)

dataset_original <- read.delim('Amazon Reviews B002CJN0H2 2018-09-27.tsv', 
                               sep = '\t', 
                               quote = '',
                               stringsAsFactors = FALSE)


# Clean up the rating and date fields
library(stringr)

dataset_original <- dataset_original %>%
  mutate(reviewRating = as.numeric(str_sub(reviewRating, start = 1, end = 1))) %>%
  mutate(reviewDate = as.Date(str_sub(reviewDate, start = 4, end = -1), '%B %d, %Y'))

# Filter to by star rating
#dataset_original <- dataset_original %>% filter(reviewRating <= 3)


# Assemble and transform the corpus
library(tm)
corpus <- Corpus(VectorSource(dataset_original$reviewText))

corpus <- tm_map(corpus, content_transformer(tolower)) # content transformation (lowercase-ifier) mapped to the text transfomer
corpus <- tm_map(corpus, removeNumbers) 
corpus <- tm_map(corpus, removePunctuation)

# remove stopwords
library(SnowballC)
corpus <- tm_map(corpus, removeWords, stopwords())

# remove unhelpful words
corpus <- tm_map(corpus, removeWords, c('filter', 'filters'))

# apply stemming and strip whitespace
#corpus <- tm_map(corpus, stemDocument) # to stem or not to stem?
corpus <- tm_map(corpus, stripWhitespace) # I think the stemDocument transform does this, too...?

# Create a bag o' words
tdm <- TermDocumentMatrix(corpus)

# Kick out sparse terms, if you'd rather plot nearly everything
#tdm <- removeSparseTerms(dtm, 0.90) 


# split out the TDM and order by frequency
tdm_matrix <- as.matrix(tdm)
tdm_vector <- sort(rowSums(tdm_matrix), decreasing = TRUE)
df <- data.frame(Word = names(tdm_vector), Freq = tdm_vector)


# Generate the word cloud
library(wordcloud)

wordcloud(words = df$Word, freq = df$Freq, min.freq = 1,
          max.words = 200, random.order = FALSE, rot.per = 0.35, 
          colors = brewer.pal(8, 'Dark2'))