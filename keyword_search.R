#' Keyword Search for Reviews
#'
#' Search keywords in review and compute the relative frequencies to adjust for healthiness.
#' @param reviews Reference objects for reviews
keyword_search = function(reviews
                          #,str_pos_1, str_pos_2, str_pos_3, str_pos_4, str_pos_5,
                          #str_neg_1, str_neg_2, str_neg_3, str_neg_4, str_neg_5
){
  x_1_pos = sum(str_count(reviews, str_pos_1))
  x_2_pos = sum(str_count(reviews, str_pos_2))
  x_3_pos = sum(str_count(reviews, str_pos_3))
  x_4_pos = sum(str_count(reviews, str_pos_4))
  x_5_pos = sum(str_count(reviews, str_pos_5))

  x_pos = c(x_1_pos,x_2_pos,x_3_pos,x_4_pos,x_5_pos)

  x_1_neg = sum(str_count(reviews, str_neg_1))
  x_2_neg = sum(str_count(reviews, str_neg_2))
  x_3_neg = sum(str_count(reviews, str_neg_3))
  x_4_neg = sum(str_count(reviews, str_neg_4))
  x_5_neg = sum(str_count(reviews, str_neg_5))

  x_neg = c(x_1_neg,x_2_neg,x_3_neg,x_4_neg,x_5_neg)

  # sum keywords counted
  sum_keywords_counted = sum(x_pos) + sum(x_neg)

  # relative frequencies x_{1}^{+},...,x_{5}^{+} and x_{1}^{-},...,x_{5}^{-}
  x_pos_rel = x_pos/sum_keywords_counted
  x_neg_rel = x_neg/sum_keywords_counted

  #results = list(x_pos_rel = x_pos_rel,
  #              x_neg_rel = x_neg_rel)
  results = c(x_pos_rel,x_neg_rel)

  return(results)
}
