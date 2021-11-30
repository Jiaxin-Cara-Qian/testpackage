#'hat_matrix
#'
#'Finds the index of the largest leverage value
#'
#'@param fit.model takes an R object, returned by lm
#'
#'@return the index of the maximum leverage  value
#'
#'
#'
#'
#'@export
#'

hat_matrix <- function(fit.model){
  X = model.matrix(fit.model)
  H = X %*% solve(t(X) %*% X) %*% t(X)
  leverage = diag(H)
  max.lev = unname(which.max(leverage))
  return(max.lev)
}
