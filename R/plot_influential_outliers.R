#'Plot Influential Outliers
#'
#'This suite of plotting functions are used to plot
#'DFFITS, Cook's Distance, and COVRATIO separately
#'
#'@usage
#'\itemize{
#'   \item{plot_dffit(fit.model)} - {plots DFFITS and labels the subjects that are outliers}
#'   \item{plot_cd(fit.model)}- {plots Cook's Distance and labels the subjects that are outliers}
#'   \item{plot_CVR(fit.model)} - {plots COVRATIO }
#'}
#'
#'@param fit.model an R object, returned by \link[stats]{lm}
#'
#'@examples
#'require(ggplot2)
#'m = lm(fev~A+H+M+S, data = demo_data)
#'plot_dffits(m)
#'plot_cd(m)
#'plot_CVR(m)
#'
#'@export
plot_dffits = function(fit.model){
  p = fit.model$rank
  n = fit.model$rank + fit.model$df.residual
  out = 2*sqrt(p/n)
  x = 1:n
  y = dffits(fit.model)
  plot_dff = data.frame(x, y)
  colnames(plot_dff) <- c("Observation", "DFFITS")
  sub = subset(plot_dff, DFFITS>out | DFFITS<(-out))
  ggplot(plot_dff, aes(Observation, DFFITS, ymax = DFFITS , ymin = 0)) +
    geom_linerange(color = "red") +
    geom_hline(yintercept = out, color = "blue") +
    geom_hline(yintercept = -out, color = "blue") +
    labs(title = "Influence Diagnostics: DFFITS") +
    geom_text(data = sub ,aes(label=Observation))
}

plot_cd = function(fit.model) {
  p = fit.model$rank
  n = fit.model$rank + fit.model$df.residual
  x = 1:n
  cd = cooks.distance(fit.model)
  plot_ckd = data.frame(x, cd)
  colnames(plot_ckd) <- c("Observation", "CooksDist")
  sub_cd = subset(plot_ckd, CooksDist > 4/n )
  ggplot(plot_ckd, aes(Observation, CooksDist, ymax = CooksDist, ymin = 0)) +
    geom_linerange(color = "red") +
    geom_hline(yintercept = 4/n , color = "blue") +
    labs(title = "Influence Diagnostics: Cook's Distance") +
    geom_text(data = sub_cd ,aes(label=Observation))
}

plot_CVR = function(fit.model){
  p = fit.model$rank
  n = fit.model$rank + fit.model$df.residual
  x = 1:n
  cvr = covratio(fit.model)
  out_up = 1+3*p/n
  out_low = 1-3*p/n
  plot_cvr = data.frame(x, cvr)
  colnames(plot_cvr) <- c("Observation", "COVRATIO")
  sub_cvr = subset(plot_cvr, abs(COVRATIO-1)>3*p/n)
  ggplot(plot_cvr, aes(Observation, COVRATIO, ymax = max(COVRATIO), ymin = min(COVRATIO))) +
    geom_point(color = "red") +
    geom_hline(yintercept = out_up , color = "blue") +
    geom_hline(yintercept = out_low , color = "blue") +
    labs(title = "Influence Diagnostics: COVRATIO")
}
