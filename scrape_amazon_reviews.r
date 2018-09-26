# Scrape Amazon Reviews

library(tidyverse)

library(rvest)
library(curl)

scrapeReviews <- function(asin, maxReview = 1000, AVPonly = FALSE, mostRecent = TRUE) {
  "
  Scrape Amazon Reviews by ASIN

  If you run this too much (or too long), Amazon will kick it out and you'll get an error message
  telling you to contact api-services-support@amazon.com to discuss automated access to Amazon data.

  asin = Amazon ASIN
  maxReview = the first X reviews to grab
  AVPonly = filter to Amazon Verified Purchases or not
  mostRecent = sort by most recent (alternative being most helpful)
  "
  
# How many seconds to sleep between pages
#    6 seconds is a good number  
  sleep_timer <- 6 
  
  reviews <- matrix(data = NA, ncol = 0, nrow = 0)
  
  if (AVPonly) {
    reviewerType = 'avp_only_reviews'
  } else {
    reviewerType = 'all_reviews'
  }
  
  if (mostRecent) {
    sortBy <- 'recent'
  } else {
    sortBy <- 'helpful'
  }

# Grab the total review count  
  url <- paste('https://www.amazon.com/product-reviews/', 
               asin,
               '/ref=cm_cr_arp_d_viewpnt_lft?ie=UTF8&reviewerType=', 
               reviewerType,
               '&pageNumber=1',
               '&sortBy=',
               sortBy,
               sep = '')  
    
  html <- read_html(url)
  Sys.sleep(sleep_timer)
  
  totalReviewCount <- html %>% 
    html_nodes('.totalReviewCount') %>%
    html_text()
  
  totalReviewCount <- gsub('[^0-9]', '', totalReviewCount) %>%
    as.numeric()

# If more reviews requested than available, adjust the maxReview number  
  if (length(totalReviewCount) > 0) {
    if (maxReview > totalReviewCount) {
      print(paste(maxReview, ' reviews requested, but only ', totalReviewCount, ' reviews available ...', sep = ''))
      maxReview <- totalReviewCount
    }
  } else {
    totalReviewCount <- 0
    print(html)
  }
    

# Iterate through the pages of reviews (10 to a page)  
  if (totalReviewCount > 0) {
    
    for (pageNumber in c(1:ceiling(maxReview / 10))) {
      
      url <- paste('https://www.amazon.com/product-reviews/', 
                   asin,
                   '/ref=cm_cr_arp_d_viewpnt_lft?ie=UTF8&reviewerType=', 
                   reviewerType,
                   '&pageNumber=',
                   pageNumber,
                   '&sortBy=',
                   sortBy,
                   sep = '')
      
      html <- read_html(url)
      Sys.sleep(sleep_timer)
      
      reviewTitle <- html %>% 
        html_nodes('.review-title') %>%
        html_text()
      
      reviewText <- html %>% 
        html_nodes('.review-text') %>%
        html_text()
      
      reviewRating <- html %>% 
        html_nodes('.review-rating') %>%
        html_text()
      
      reviewDate <- html %>% 
        html_nodes('.review-date') %>%
        html_text()
      
      if (length(reviewTitle) > length(reviewText)) {
        if (length(reviewText) > 0) {
          reviewTitle <- reviewTitle[(length(reviewTitle) - length(reviewText) + 1):length(reviewTitle)]
          reviewRating <- reviewRating[(length(reviewRating) - length(reviewText) + 1):length(reviewRating)]
          reviewDate <- reviewDate[(length(reviewDate) - length(reviewText) + 1):length(reviewDate)]
        } else {
          reviewTitle <- reviewText
          reviewRating <- reviewText
          reviewDate <- reviewText
        }
        
      }
      
      page <- cbind(reviewTitle, reviewText, reviewRating, reviewDate)
      
      print(paste('Scraping page ', pageNumber, ' of ', ceiling(totalReviewCount / 10), ', found ', nrow(page), ' reviews.', sep = ''))
      
      if (length(reviews) > 0) {
        reviews <- rbind(reviews, page)
      } else {
        reviews <- page
      }
      
    }
    
  }
  
# Trim it up if less than a full page requested
  reviews <- as.data.frame(reviews)
  if (maxReview < nrow(reviews)) {
    reviews <- reviews[1:maxReview, ]
  }
  
  return(reviews)
  
}


#asin <- 'B07FWHJYS4' # ASIN for a Panera coffee pod, because that's what I was drinking when I wrote this script
asin <- 'B002CJN0H2' # ASIN for a furnace filter that has a few thousand reviews
reviews <- scrapeReviews(asin, maxReview = 4000)

# Save the scraped reviews to a file
filename <- paste('Amazon Reviews ', asin, ' ', format(Sys.Date(), '%Y-%m-%d'), '.tsv', sep = '')
write.table(reviews, file = filename, sep = '\t')

# Clean up the rating and date fields
library(stringr)

reviews <- reviews %>%
  mutate(reviewRating = as.numeric(str_sub(reviewRating, start = 1, end = 1)),
         reviewDate = as.Date(str_sub(reviewDate, start = 4, end = -1), '%B %d, %Y'))