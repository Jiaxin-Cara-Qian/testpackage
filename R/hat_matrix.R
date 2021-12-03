#'hat_matrix
#'
#'Finds the index of the largest leverage value
#'
#'@param fit.model takes an R object, returned by \link[stats]{lm}
#'
#'@return  A list containg the following components:
#' \itemize{
#'   \item{maximum hi} - {which subject has the maximum value of the leverage}
#'   \item{hi>2*mean(H)} - {what subjects are outliers in the X space, and their leverage values}
#' }
#'
#'@examples
#'##Use demo_data (See ?demo_data for more information about the dataset)
#'m = lm(fev~A+H+M+S, data = demo_data)
#'hat_matrix(m)
#'hat_matrix(m)[1]
#'hat_matrix(m)[2]
#'
#'
#'@export
hat_matrix = function(fit.model){
  X = model.matrix(fit.model)
  H = X %*% solve(t(X) %*% X) %*% t(X)
  leverage = diag(H)
  max.lev = leverage[which.max(leverage)]
  lev.gtmean = leverage[which(leverage>2*mean(leverage))]
  result_lev = list(max.lev, lev.gtmean)
  names(result_lev) = c("maximum hi", "hi>2*mean(H)")
  return(result_lev)
}
