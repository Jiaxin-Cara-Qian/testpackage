#'res_3
#'
#'res_3 computes three types of residuals from a linear regression model.
#'These residual types are standardized resisuals, internally studentized residuals,
#'and externally studentized residuals.
#'
#'@usage res_3(input_data, fit.model ,r = c("z", "int", "ex"))
#'
#'@param input_data a data frame containing the variables in th model
#'@param fit.model an R object, returned by \link[stats]{lm}
#'@param r what type of resisuals will be computed: "z" is standardized resisuals,
#'"int" is internally studentized resisuals, and "ex" is externally studentized resisuals
#'
#'@return if standardized resisuals are computed, a list of two elements will be returned
#'\itemize{
#'   \item{percentage cutoff} - {If the model assumptions are correct, z will follow a standard normal distribution
#'   with mean 0 and variance 1. This can be tested for whether the model follows normal distribution.}
#'   \item{standardized resisuals} - {all the computed standardized residuals}
#' }
#'
#'@examples
#'res_3(demo_data, lm(fev~A+H+M+S, data = demo_data), r = "z")
#'res_3(demo_data, lm(fev~A+H+M+S, data = demo_data), r = "int")
#'res_3(demo_data, lm(fev~A+H+M+S, data = demo_data), r = "ex")
#'
#'
#'@export
res_3 = function(input_data, fit.model ,r = c("z", "int", "ex")){
  n = nrow(input_data)
  p = fit.model$rank # dimensions
  fit.res = fit.model$residuals
  sigma = sqrt( sum(fit.res^2)/(n-p) )
  X = model.matrix(fit.model)
  if(r == "z"){
    z.res = fit.res/sigma #standardized residual
    #find z residuals distribution percentage cutoff
    cutoff = c(1, 2, 2.5)
    per = as.numeric(lapply(cutoff, FUN = function(x){mean(as.numeric(abs(z.res)>x))*100}))
    names(per) = c("Pr(>1)", "Pr(>2)", "Pr(>2.5)")
    result_z = list(per, z.res)
    names(result_z) = c("percentage cutoff", "standardized resisuals")
    return(result_z)

  } else if(r == "int") {
    int.res = fit.res/(sigma*sqrt(1-hatvalues(fit.model)))
    return(int.res)

  } else if (r == "ex"){
    matrix.data = as.matrix(input_data)
    name_y = as.character(fit.model$term[[2]])
    y = matrix.data[,name_y]
    df_res = n-p
    ex_model = .Call(stats:::C_Cdqrls, X, y, 1e-7, FALSE)
    rstand = ex_model$residuals/sqrt(sum(ex_model$residuals^2)/df_res)/sqrt(1-hatvalues(fit.model))
    rrr =  rstand * sqrt((df_res - 1)/( df_res - rstand^2))
    return(rrr)
  }
}
