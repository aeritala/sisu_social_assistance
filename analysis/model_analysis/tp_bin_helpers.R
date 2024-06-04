
testError <- function(zrep, z) {
  prop_cor <- mean(zrep != z)
  return(prop_cor)
}

#' Calculates Cohen's Kappa
#'
#' @param zrep Predicted values
#' @param z Observed values
#'
#' @return Cohen's Kappa
cohenKappa <- function(zrep, z) {
  p_o <- 1 - testError(zrep, z)
  p_yes <- mean(zrep == 1) * mean(z == 1)
  p_no <- mean(zrep == 0) * mean(z == 0)
  p_e <- p_yes + p_no
  kappa <- (p_o - p_e)/(1 - p_e)
  return(kappa)
}

#' Calculates true positive rate
#' @param zrep predicted values
#' @param z observed values
#' @return true positive rate
truePosRate <- function(zrep, z) {
  prop_truepos <- mean(zrep[z == 1] == 1)
  return(prop_truepos)
}

#' Calculates false positive rate
#' @param zrep predicted values
#' @param z observed values
#' @return false positive rate
falsePosRate <- function(zrep, z) {
  prop_falsepos <- mean((zrep[z == 0] == 1))
  return(prop_falsepos)
}

#' Calculates a 
#'
#' @param l 
#' @param zrep 
#' @param z 
#' @param f 
#'
#' @return
calcTestError <- function(l, zrep, z) {
  zpred <- ifelse(zrep >= l, 1, 0)
  est <- testError(zpred, z)
  res <- c(l = l, est = est)
  return(res)
}

#' Calculates a 
#'
#' @param l 
#' @param zrep 
#' @param z 
#' @param f 
#'
#' @return
calcCohenKappa <- function(l, zrep, z) {
  zpred <- ifelse(zrep >= l, 1, 0)
  est <- cohenKappa(zpred, z)
  res <- c(l = l, est = est)
  return(res)
}

#' Calculates a ratio of false positives and
#' false negatives
#' @param zrep predicted values
#' @param z observed values
#' @return a named vector with true positive rate (tprate) and
#' false positive rate (fprate)
calcErrors <- function(zrep, z) {
  tprate <- truePosRate(zrep, z)
  fprate <- falsePosRate(zrep, z)
  res <- c(tprate = tprate, fprate = fprate)
}

#' Calculates a ratio of false positives and
#' false negatives at limit l
#' @param zrep predicted values
#' @param z observed values
#' @param l classification limit
#' @return a named vector containing the
#'        limit (l) used, true positive rate (tprate), and
#'        false positive rate (fprate)
errorRatio <- function(l, zrep, z) {
  zpred <- ifelse(zrep >= l, 1, 0)
  prop <- calcErrors(zpred, z)
  res <- c(l = l, prop)
  return(res)
}

#' Calculates error ratios needed to plot
#' an ROC curve
#' @param zrep posterior predictive samples
#' @param z observed values
#' @return
calcStatistic <- function(zrep, z, f, limits) {
  res <- lapply(limits, f, zrep, z) %>% 
    Reduce(rbind, .) %>% 
    data.frame(., row.names = NULL)
  return(res)
}

#' Plots the ROC curve for a logistic regression model
#' fit with brms
#' @param fit brms -fit
#' @param z observed values
#' @param seed random seed
#' @param ndraws number of posterior draws
#' @param len number of points at which the
#'        true positive and false negative ratio is
#'        calculated
#' @return
ROCPlot <- function(newdata, points) {
  
  plotdata <- newdata %>% 
    group_by(l) %>% 
    summarise(
      tprate = mean(proptp),
      fprate = mean(propfp)#,
      # lwr_ci_tp = quantile(proptp, 0.01),
      # lwr_ci_fp = quantile(propfp, 0.01),
      # upr_ci_tp = quantile(proptp, 0.99),
      # upr_ci_fp = quantile(propfp, 0.99)
    )
  
  rocplot <- ggplot(plotdata, aes(y = tprate, x = fprate)) +
    geom_point(data = points, aes(x = fprate, y = tprate, shape = Classifier),
               col = "cornflowerblue") +
    scale_shape_discrete(name = "Classifier") +
    geom_line() +
    geom_abline(linetype = "dashed") +
    coord_fixed() +
    labs(y = "True positive rate",
         x = "False positive rate") +
    scale_x_continuous(expand = rep(0.008, 2)) +
    scale_y_continuous(expand = rep(0.007, 2)) +
    theme_bw() +
    theme(
      text = element_text(size = 8.5),
      axis.title = element_text(size = 8),
      axis.text = element_text(size = 8),
      legend.title = element_text(size = 8),
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.4, "cm"),
      legend.position = "top")
  
  return(rocplot)
}

classPlot <- function(newdata, support) {
  
  # scaleFun <- function(x) sprintf("%.1f", x)
  
  class_plot <- ggplot(res_class, aes(x = estimate*100)) +
    geom_density(fill = "lightgray") +
    geom_vline(data = res_class %>% filter(statistic == "test_error"),
               aes(xintercept = support$sisu_te),
               col = "blue",
               linetype = 2) +
    geom_vline(data = res_class %>% filter(statistic == "test_error"),
               aes(xintercept = support$dummy_te),
               col = "red",
               linetype = 3) +
    geom_vline(data = res_class %>% filter(statistic == "false_neg"),
               aes(xintercept = support$sisu_fn),
               col = "blue",
               linetype = 2) +
    geom_vline(data = res_class %>% filter(statistic == "false_pos"),
               aes(xintercept = support$sisu_fp),
               col = "blue",
               linetype = 2) +
    labs(y = "Density",
         x = "Percentage (%)") +
    facet_grid(rows = vars(optim_stat),
               cols = vars(statistic),
               scales = "free",
               labeller = labeller(statistic = c("false_neg" = "False negative rate",
                                                 "false_pos" = "False positive rate",
                                                 "test_error" = "Test error rate"),
                                   optim_stat = c("cohens_kappa" = "Cohen's Kappa",
                                                  "test_error" = "Test error rate"))) +
    theme_bw()
  
  return(class_plot)
  
}

