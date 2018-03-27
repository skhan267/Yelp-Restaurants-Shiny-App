#' Classification Formula for Fast-Food Restaurants
#'
#' A formula to compute health rank for Fast Food Restaurants.
#' @param x_rel XML reference object from XML package
FF = function(x_rel){
  if((sum(x_rel[1:5])-sum(x_rel[6:10])) <= 0){
    H_FF = 3 - 3*abs(sum(x_rel[1:5])-sum(x_rel[6:10]))
  } else{
    H_FF = 3 + 7*abs(sum(x_rel[1:5])-sum(x_rel[6:10]))
  }
  return(H_FF)
}
