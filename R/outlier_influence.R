#'outlier_influence
#'
#'outlier_influence computes three diagnostic statistics for measuring influential points:
#' \describe{
#'   \item{DIFFTS}{Difference in the fitted value when ith observation is removed}
#'   \item{Cook's Distance}{shows the influence of each observation on the fitted response values}
#'   \item{COVRATIO}{influence on variance-covariance matrix}
#' }
#'
#'@usage
#'outlier_influence(input_data, fit.model,
#'   option = c("dffits", "cd" , "cvr"),
#'     high.influence = FALSE)
#'
#'@param input_data a data frame containing the variables in th model
#'@param fit.model takes an R object, returned by \link[stats]{lm}
#'@param option which measure of influence diagnostics should be computed.
#'"dffits" is for DFFITS, "cd" is for Cook's distance,
#'"cvr" is for COVRATIO
#'@param high.influence (Default)FALSE; if TRUE, influential observation along with their
#'respective diagnostic statistics will be returned.
#'
#'
#'
#'@return  If high.influence is set TRUE, each diagnostic statistic will return
#'a list containing two elements.
#'\itemize{
#'   \item{dffits, Cook's Distance, COVRATIO} - {depends on which option is chosen}
#'   \item{outliers} - {which observations are considered influential}
#' }
#'
#'@examples
#'##Use demo_data (See ?demo_data for more information about the dataset)
#'m = lm(fev~A+H+M+S, data = demo_data)
#'outlier_influence(demo_data, m, option = c("dffits"))
#'outlier_influence(demo_data, m, option = c("cd"))
#'outlier_influence(demo_data, m, option = c("cvr"), high.influence = TRUE)
#'
#'@export
outlier_influence = function(input_data, fit.model, option = c("dffits", "cd" , "cvr"), high.influence = FALSE){
  ex.res = res_3(input_data, fit.model, r="ex")
  int.res = res_3(input_data, fit.model, r="int")
  fit.res = fit.model$residuals
  n = nrow(input_data)
  p = fit.model$rank
  MSE = sum(fit.res^2)/(n-p)
  X = model.matrix(fit.model)
  lev=hatvalues(fit.model)
  if (option == "dffits"){
    dff = ex.res*sqrt(lev/(1-lev))
    out.dff = dff[which(abs(dff)>2*sqrt(p/n))]
    result_dff = list(dff, out.dff)
    names(result_dff) = c("dffits", "outliers")
    if (high.influence){
      return(result_dff)
    } else {
      return(dff)
    }
  } else if ( option == "cd") {
    cd = int.res^2/p * lev/(1-lev)
    out.cd = cd[which(cd>4/n)]
    result_cd = list(cd, out.cd)
    names(result_cd) = c("Cook's Distance", "outliers")
    if (high.influence){
      return(result_cd)
    } else {
      return(cd)
    }
  } else if (option == "cvr") {
    matrix.data = as.matrix(input_data)
    name_y = as.character(fit.model$term[[2]])
    y = matrix.data[,name_y]
    df_res = n-p-1
    ex_model = .Call(stats:::C_Cdqrls, X, y, 1e-7, FALSE)
    MSE_i = (fit.res/(ex.res * sqrt(1-lev)))^2
    cvr = (MSE_i/MSE)^p*(1/(1-lev))
    out.cvr = cvr[which((abs(cvr-1)>3*p/n))]
    result_cvr = list(cvr, out.cvr)
    names(result_cvr) = c("COVRATIO", "outliers")
    if (high.influence){
      return(result_cvr)
    } else {
      return(cvr)
    }
  }
}
