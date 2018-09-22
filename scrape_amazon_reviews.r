# Scrape Amazon Reviews

library(rvest)
library(curl)

scrapeReviews <- function(asin, maxReview = 100, AVPonly = FALSE, mostRecent = TRUE) {
  "
  Scrape Amazon Reviews by ASIN

  If you run this too much (or too long), Amazon will kick it out and you'll get an error message
  telling you to contact api-services-support@amazon.com to discuss automated access to Amazon data.

  asin = Amazon ASIN
  maxReview = the first X reviews to grab
  AVPonly = filter to Amazon Verified Purchases or not
  mostRecent = sort by most recent (alternative being most helpful)

  "
  
  
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
  
  totalReviewCount <- html %>% 
    html_nodes('.totalReviewCount') %>%
    html_text() %>%
    as.numeric()
  
  
  if (length(totalReviewCount) > 0) {
    if (maxReview > totalReviewCount) {
      print(paste(maxReview, ' reviews requested, but only ', totalReviewCount, ' reviews available ...', sep = ''))
      maxReview <- totalReviewCount
    }
  } else {
    totalReviewCount <- 0
    print(html)
  }
    
  
  if (totalReviewCount > 0) {
    
    for (pageNumber in c(1:ceiling((maxReview - 1) / 10))) {
      
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
      
      reviewTitle <- html %>% 
        html_nodes('.review-title') %>%
        html_text()
      
      reviewText <- html %>% 
        html_nodes('.review-text') %>%
        html_text()
      
      reviewRating <- html %>% 
        html_nodes('.review-rating') %>%
        html_text()
      
      if (length(reviewTitle) > length(reviewText)) {
        if (length(reviewText) > 0) {
          reviewTitle <- reviewTitle[(length(reviewTitle) - length(reviewText) + 1):length(reviewTitle)]
          reviewRating <- reviewRating[(length(reviewRating) - length(reviewText) + 1):length(reviewRating)]
        } else {
          reviewTitle <- reviewText
          reviewRating <- reviewText
        }
        
      }
      
      #    print(paste('Rating:', nrow(reviewRating), '-- Title:', nrow(reviewTitle), '-- Text:', nrow(reviewText)))
      
      page <- cbind(reviewTitle, reviewText)
      page <- cbind(page, reviewRating)
      
      print(paste('Scraping page ', pageNumber, ', found ', nrow(page), ' reviews.', sep = ''))
      
      if (length(reviews) > 0) {
        reviews <- rbind(reviews, page)
      } else {
        reviews <- page
      }
      
    }
    
  }
  
  reviews <- as.data.frame(reviews)
  if (maxReview < nrow(reviews)) {
    reviews <- reviews[1:maxReview, ]
  }
  
  return(reviews)
  
}


asin <- 'B07FWHJYS4' # ASIN for a Panera coffee pod, because that's what I was drinking when I wrote this script
reviews <- scrapeReviews(asin, maxReview = 65)