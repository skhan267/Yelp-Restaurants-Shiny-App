#' Set-Up for Keyword Reviews Computation
#'
#' A paralelized function to compute the review variables as an input to the keyword search function.
#' @param i Objects for iterator reference
get_restaurant = function(i){
  reviews = character(0)
  #  end_review_html = restaurant_reviews[i]

  raw_number_review_pages = read_html(paste(restaurant_reviews[i],"start=0", sep="")) %>%
    html_nodes(css = selector_number_review_pages) %>%
    html_text()

  raw_number_review_pages = unique(na.omit(as.numeric(
    unlist(strsplit(unlist(raw_number_review_pages), "[^0-9]+")))))

  if(length(raw_number_review_pages) == 2){
    number_review_pages = raw_number_review_pages[2]
  } else{
    number_review_pages = raw_number_review_pages[1]
  }

  if(is.na(number_review_pages)){next}

  iterator = paste(as.character(seq(0,number_review_pages*20, by = 20)))

  for(j in seq_len(number_review_pages)){
    url_reviews = read_html(paste(restaurant_reviews[i],"start=",iterator[j], sep = ""))

    reviews =c(reviews, html_nodes(x = url_reviews, css = selector_reviews) %>%
                 html_text())
  }

  keyword_search(reviews = reviews)
}
