#' Classification Formula for Non Fast-Food Restaurants
#'
#' A formula to compute health rank for Non Fast Food Restaurants.
#' @inheritParams FF
NFF = function(x_rel){
  if((sum(x_rel[1:5])-sum(x_rel[6:10])) <= 0){
    H_NFF = 7 - 7*abs(sum(x_rel[1:5])-sum(x_rel[6:10]))
  } else{
    H_NFF = 7 + 3*abs(sum(x_rel[1:5])-sum(x_rel[6:10]))
  }
  return(H_NFF)
}
