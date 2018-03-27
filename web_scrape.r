#' Web-Scrapping the Data
#'
#' Scrapping the Data from Yelp for the Urbana- Champaign Area

  # Start the clock!
  ptm <- proc.time()

  ##############################
  # include necessary packages #
  ##############################

  library(dplyr)
  library(rvest)
  library(ggmap)
  library(leaflet)
  library(RColorBrewer)
  library(stringr)
  library(parallel)


  ##########################################################
  # get number of html pages with restaurant names n       #
  # we will need this code later when we extract all names #
  ##########################################################

  url_bla = read_html("https://www.yelp.com/search?find_desc=Restaurants&find_loc=Champaign%2C+IL&ns=1")
  selector_bla = ".pagination-results-window"
  k = html_nodes(x = url_bla, css = selector_bla) %>%
    html_text()
  k = unique(na.omit(as.numeric(unlist(strsplit(unlist(k), "[^0-9]+")))))
  n = ceiling(k[3]/k[2])


  ##################################################################################
  # get restaurant names in Urbana-Champaign for the first 100 restaurants on Yelp #
  # --> n= 100/10 = 10                                                             #
  ##################################################################################
  n = 5
  restaurant_names = character(0)
  url_res_start =
    "https://www.yelp.com/search?find_desc=Restaurants&find_loc=Champaign,+IL&start="
  url_res_end = as.character(seq(0,ceiling((k[3]-k[2])/k[2]))*k[2])
  selector_restaurant_names = "#super-container .js-analytics-click span"
  for (i in seq_len(n)){
    url_restaurant_names = read_html(paste(url_res_start,url_res_end[i], sep=""))
    restaurant_names = c(restaurant_names,
                         html_nodes(x = url_restaurant_names,
                                    css = selector_restaurant_names) %>%
                           html_text())
    print(i)
  }
  restaurant_names = restaurant_names[-(1 + seq(0,n-1)*(k[2]+1))]
  restaurant_names

  ############################
  # get restaurant addresses #
  ############################

  #n=10
  restaurant_address = character(0)
  url_res_start =
    "https://www.yelp.com/search?find_desc=Restaurants&find_loc=Champaign,+IL&start="
  url_res_end = as.character(seq(0,ceiling((k[3]-k[2])/k[2]))*k[2])
  selector_restaurant_address = ".secondary-attributes"
  for (i in seq_len(n)){
    url_restaurant_address = read_html(paste(url_res_start,url_res_end[i], sep=""))
    restaurant_address = c(restaurant_address,
                           html_nodes(x = url_restaurant_address,
                                      css = selector_restaurant_address) %>%
                             html_text())
    print(i)
  }
  restaurant_address = restaurant_address[-(1 + seq(0,n-1)*(k[2]+1))]
  restaurant_address = gsub("\\n","", restaurant_address)
  restaurant_address = gsub("  ","", restaurant_address)
  restaurant_address = sub("Phone.*","", restaurant_address)
  restaurant_address = gsub("St","St, ", restaurant_address)
  restaurant_address

  ######################
  # get restaurant key #
  ######################
  #n=5
  restaurant_key = character(0)
  url_res_start =
    "https://www.yelp.com/search?find_desc=Restaurants&find_loc=Champaign,+IL&start="
  url_res_end = as.character(seq(0,ceiling((k[3]-k[2])/k[2]))*k[2])
  selector_restaurant_key = "span.category-str-list"
  for (i in seq_len(n)){
    url_restaurant_key = read_html(paste(url_res_start,url_res_end[i], sep=""))
    restaurant_key = c(restaurant_key,
                       html_nodes(x = url_restaurant_key,
                                  css = selector_restaurant_key) %>%
                         html_text())
    print(i)
  }
  restaurant_key = restaurant_key[-(1 + seq(0,n-1)*(k[2]+1))]
  restaurant_key = gsub("\\n","", restaurant_key)
  restaurant_key = gsub("  ","", restaurant_key)
  restaurant_key

  fastfood = grepl("Fast Food", restaurant_key)
  pubs = grepl("Pubs", restaurant_key)
  chips = grepl("Fish & Chips", restaurant_key)
  bars = grepl("Bar", restaurant_key)
  ice = grepl("Ice Cream & Frozen Yogurt", restaurant_key)
  delis = grepl("Delis", restaurant_key)
  foodtruck = grepl("Food Truck", restaurant_key)


  Initial_Classification = fastfood | pubs | chips | bars | ice | delis | foodtruck

  sum(Initial_Classification)
  for (i in seq_len(k[3])) {
    if (Initial_Classification[i] == TRUE && !is.na(Initial_Classification[i])) {
      Initial_Classification[i] = "Unhealthy"
    }
    else {
      Initial_Classification[i] = "Healthy"
    }
  }
  Initial_Classification



  ###########################################################################
  # get array with url endings of review pages for 100 restaurants --> n=10 #
  ###########################################################################

  #n = 5
  restaurant_url = character(0)
  url_res_start =
    "https://www.yelp.com/search?find_desc=Restaurants&find_loc=Champaign,+IL&start="
  url_res_end = as.character(seq(0,ceiling((k[3]-k[2])/k[2]))*k[2])
  for (i in seq_len(n)){
    init_url = html_session(paste(url_res_start,url_res_end[i], sep=""))
    for(j in seq_len(10)){
      raw_url = follow_link(init_url,i=restaurant_names[-10 + 10*i + j])$url
      restaurant_url = c(restaurant_url, raw_url)
    }
    print(i)
  }
  restaurant_reviews = restaurant_url
  restaurant_reviews = sub('(?<=\\?).*$', '', restaurant_reviews, perl=TRUE)
  restaurant_reviews

  # n = 5
  # restaurant_reviews = character(0)
  # url_review_start =
  #   "https://www.yelp.com/search?find_desc=Restaurants&find_loc=Champaign,+IL&start="
  # url_review_end = as.character(seq(0,ceiling((k[3]-k[2])/k[2]))*k[2])
  # selector_restaurant_review = ".nowrap"
  # for (i in seq_len(n)){
  #   url_restaurant_reviews = read_html(paste(url_review_start,url_review_end[i], sep=""))
  #   restaurant_reviews = c(restaurant_reviews,
  #                          html_nodes(x = url_restaurant_reviews,
  #                                     css = selector_restaurant_review) %>%
  #                            html_attr("href"))
  #   print(i)
  # }
  # restaurant_reviews = restaurant_reviews[!is.na(restaurant_reviews)]
  # restaurant_reviews = restaurant_reviews[-(1 + seq(0,n-1)*(k[2]+1))]
  # restaurant_reviews = sub('(?<=\\?).*$', '', restaurant_reviews, perl=TRUE)
  # restaurant_reviews


  ###############################################################
  # perform keyword search and compute the relative frequencies #
  ###############################################################

  # positive keywords
  str_pos_1 = "healthy"
  str_pos_2 = "vegetarian"
  str_pos_3 = "clean"
  str_pos_4 = "seasonal"
  str_pos_5 = "fruit"

  # negative keywords
  str_neg_1 = "unhealthy"
  str_neg_2 = "greasy"
  str_neg_3 = "oily"
  str_neg_4 = "fatty"
  str_neg_5 = "sticky"

  # absolute frequencies x_{1}^{+},...,x_{5}^{+} and x_{1}^{-},...,x_{5}^{-}
  keyword_search(reviews)

  #######################################################
  # classification function for "Fast Food" restaurants #
  #######################################################


  FF(x_rel)

  ############################################################
  # classification function for " Non Fast Food" restaurants #
  ############################################################


  NFF(x_rel)

  #############################################################################
  # get matrix with relative keyword frequencies for 100 restaurants --> n=10 #
  #############################################################################

  beginning_review_html = "https://www.yelp.com/"
  selector_reviews = ".review-content p"
  selector_number_review_pages = ".page-of-pages"
  x_rel_matrix = matrix(NA, nrow =10, ncol = length(restaurant_reviews))

  # create cluster
  cores = detectCores()
  cl = makeCluster(cores - 1)

  clusterExport(cl, varlist = c('restaurant_reviews',
                                'beginning_review_html',
                                'selector_reviews',
                                'selector_number_review_pages',
                                'keyword_search',
                                # positive keywords
                                'str_pos_1',
                                'str_pos_2',
                                'str_pos_3',
                                'str_pos_4',
                                'str_pos_5',

                                # negative keywords
                                'str_neg_1',
                                'str_neg_2',
                                'str_neg_3',
                                'str_neg_4',
                                'str_neg_5' ))

  clusterEvalQ(cl, { library('rvest'); library('stringr') })

  # here is a function that will be parallelized
  get_restaurant(i)

  results = parLapply(cl, seq_along(restaurant_reviews), fun =get_restaurant) #seq_along(restaurant_reviews))
  x_rel_matrix = matrix(unlist(results), ncol = (10*n), nrow = 10, byrow = FALSE)

  # errors in which columns --> by now: do it manually for these columns
  NA_x_rel_matrix = which(is.na(apply(x_rel_matrix, 2, sum)))

  # stop clusters
  stopCluster(cl)

  ##################################################################
  # Classification for "Fast Food" and "Non Fast Food" restaurants #
  ##################################################################
  #names = read.csv("names.csv")
  #names = names$FF.[1:(10*n)]


  classification = numeric(10*n)
  FF_vec = which(grepl("Unhealthy", Initial_Classification))
  NFF_vec = which(grepl("Healthy", Initial_Classification))

  # remove NA columns from matrix of relative frequencies (word counts)
  # --> set them to -1

  x_rel_matrix[ , which( colSums(is.na(x_rel_matrix)) ==
                           nrow(x_rel_matrix) ) ] = -1

  classification[FF_vec] = round(apply(as.matrix(x_rel_matrix[,FF_vec],
                                                 ncol=length(FF_vec)), 2, FF),
                                 digits = 2)
  classification[NFF_vec] = round(apply(as.matrix(x_rel_matrix[,NFF_vec],
                                                  ncol = length(NFF_vec)), 2, NFF),
                                  digits = 2)

  # less than 20 reviews --> NA (because not enough data)
  # classification[NA_x_rel_matrix] = NA

  # Remove errors --> set to NA
  for(i in seq_len(length(classification))){
    if((classification[i] >10) | (classification[i] < 0)){
      classification[i] = NA
    }
  }


  ##############################################################
  # create data frame with restaurant names and classification #
  ##############################################################
  geo = geocode(restaurant_names)
  address_long = geo$lon
  address_lat = geo$lat


  pop_ups = character(0)
  for (i  in seq_len(10*n)){
    pop_ups[i] = paste(restaurant_names[i],
                       "<br/>Rank: ",round(classification[i], digits=1),
                       "/10", "<br/>", restaurant_url[i], sep="")
  }

  restaurant_classification =
    as.data.frame(cbind(restaurant_names,
                        restaurant_address,
                        address_long,
                        address_lat,
                        restaurant_key,
                        restaurant_url,
                        pop_ups,
                        Initial_Classification,
                        classification))
  colnames(restaurant_classification) = c("Names", "Address",
                                          "address_long", "address_lat",
                                          "Type", "URL", "pop_ups","Initial Ranking",
                                          "Health Ranking")

  write.csv(restaurant_classification, file="output.csv", row.names = FALSE)


  # Elmhurst
  # Hinsdale

  # Stop the clock
  proc.time() - ptm


