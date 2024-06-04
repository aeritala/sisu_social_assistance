
calcLogLikInSegments <- function(fit, newdata) {
  n_postp <- sum(fit$fit@sim$n_save - fit$fit@sim$warmup2)
  new_loglik <- matrix(numeric(0),
                       ncol = nrow(newdata),
                       nrow = n_postp)
  intervals <- seq(1, nrow(newdata), length.out = 10) %>% round()
  for (i in seq(1, (length(intervals)-1), 1)) {
    newdata_i <- newdata[intervals[i]:intervals[i+1],]
    new_loglik_i <- log_lik(fit, newdata = newdata_i, cores = 3)
    new_loglik[,intervals[i]:intervals[i+1]] <- new_loglik_i
    rm(new_loglik_i)
  }
  return(new_loglik)
}

calcTwoPartLogLik <- function(y,
                              loglik_binreg,
                              loglik_basereg) {
  
  loglik_tp <- matrix(numeric(0),
                      nrow = nrow(loglik_binreg),
                      ncol = ncol(loglik_binreg))
  basereg_index <- 1
  for (i in 1:ncol(loglik_binreg)) {
    if (y[i] == 0) {
      loglik_tp[,i] <- loglik_binreg[,i]
    } else {
      loglik_tp[,i] <- loglik_binreg[,i] + loglik_basereg[,basereg_index]
      basereg_index <- basereg_index + 1
    }
  }
  return(loglik_tp)
  
}

rowOperLPPD <- function(x) {
  n <- length(x)
  maxx <- max(x)
  w <- x - maxx
  res <- maxx - log(n) + log(sum(exp(w)))
  return(res)
}

calcPropIncorrect <- function(zrep, z) {
  prop_cor <- mean(zrep != z)
  return(prop_cor)
}

calcFalseNeg <- function(zrep, z) {
  prop_falseneg <- mean(zrep[z == 1] == 0)
  return(prop_falseneg)
}

calcFalsePos <- function(zrep, z) {
  prop_falsepos <- mean(zrep[z == 0] == 1)
  return(prop_falsepos)
}

#' Calculates Cohen's Kappa
#'
#' @param zrep Predicted values
#' @param z Observed values
#'
#' @return Cohen's Kappa
cohenKappa <- function(zrep, z) {
  p_o <- 1 - calcPropIncorrect(zrep, z)
  p_yes <- mean(zrep == 1) * mean(z == 1)
  p_no <- mean(zrep == 0) * mean(z == 0)
  p_e <- p_yes + p_no
  kappa <- (p_o - p_e)/(1 - p_e)
  return(kappa)
}

#' Function to calculate an F-score for a sample given as input.
#' The calculated F-score is the F_1-score, so-called
#' harmonic mean of precision and recall.
#'
#' @param yrep Predicted values
#' @param y Observed values
#'
#' @return F_1 -score
FScore <- function(yrep, y, beta = 1) {
  t <- table(yrep, y)
  n_tp <- t[2,2]
  n_fn <- t[1,2]
  n_fp <- t[2,1]
  c <- 1 + beta^2 
  fscore <- (c*n_tp)/(c*n_tp + (beta^2)*n_fn + n_fp)
  return(fscore)
}

#' Predict class for given posterior predictive expected values based on 
#' limit given as input. If the expected value is higher or equal than l, the 
#' observation is predicted 1 and else 0
#'
#' @param yrep Vector of expected values of the posterior predictive distribution
#' @param l Classification limit between 0 and 1, by default 0.5
#'
#' @return
#' @export
#'
#' @examples
predictClass <- function(yrep, l = 0.5) {
  p <- ifelse(yrep >= l, 1, 0)
  return(p)
}

#' Calculate distribution of a classification performance measure
#' (which can be calculated with function f), over classification
#' limits given as input
#'
#' @param pp matrix of expected values of the posterior predictive distribution
#' @param y vector of observed classes
#' @param limits vector of classification limits to be tested
#' @param long if TRUE the final result is returned in a long format,
#'              and if FALSE the result is returned in a wide format
#' @param f function, which takes as a first argument a vector of predicted classes
#'          and a vector of true classes as the second argument
#'
#' @return a matrix consisting of a vector of scores for each
#'        specified limit, parameter long controls whether the
#'        output is in wide (default) or long format
#' @export
#'
#' @examples
calcScores <- function(pp, y, limits, long = FALSE, f) {
  scores <- matrix(0, nrow = nrow(pp), ncol = length(limits))
  colnames(scores) <- paste0("l", limits)
  for (l in seq_along(limits)) {
    for (i in seq_along(1:nrow(pp))) {
      yrep <- predictClass(pp[i,], limits[l])
      scores[i, l] <- f(yrep, y)
    }
  }
  if (isTRUE(long)) {
    scores <- pivot_longer(as.data.frame(scores),
                           cols = everything(),
                           values_to = "score",
                           names_to = "limit")
  }
  return(scores)
}

findLimit <- function(epred, z, limits, optim_stat) {
  switch(optim_stat,
         "test_error" = {
           f = calcPropIncorrect
           which.f = which.min
         },
         "fscore" = {
           f = FScore
           which.f = which.max
         },
         "cohenkappa" = {
           f = cohenKappa
           which.f = which.max
         })
  scores <- calcScores(pp = epred,
                       y = z,
                       limits = limits,
                       f = f)
  mean_scores <- colMeans(scores)
  l <- limits[which.f(mean_scores)]
  return(l)
}

calcPropZeros <- function(zrep) {
  zrep_prop_zeros <- mean(zrep == 0)
  return(zrep_prop_zeros)
}

calcKSTest <- function(yrep, y) {
  ks_test <- ks.test(yrep, y) %>% suppressWarnings()
  return(ks_test$statistic)
}

calcRMSE <- function(yrep, y) {
  rmse <- sqrt(mean((y - yrep)^2))
  return(rmse)
}

calcSummary <- function(new_vector, row_name) {
  new_summary <- data.frame("mean" = mean(new_vector),
                   "lwr_ci" = quantile(new_vector, 0.025, names = FALSE),
                   "upr_ci" = quantile(new_vector, 0.975, names = FALSE))
  rownames(new_summary) <- row_name
  return(new_summary)
}

calcSummaryNV <- function(new_vector) {
  new_summary <- c("mean" = mean(new_vector),
                   "lwr_ci" = quantile(new_vector, 0.025, names = FALSE),
                   "upr_ci" = quantile(new_vector, 0.975, names = FALSE))
  return(new_summary)
}

calcBaseRegStats <- function(fit_basereg, newdata) {
  
  # response
  y <- newdata$toimtuki_data
  
  # lppd
  lppd_basereg <- log_lik(fit_basereg,
                          newdata = newdata) %>%
    apply(., 2, rowOperLPPD) 
  
  lppd_basereg_sum <- lppd_basereg %>% 
    sum() %>%
    calcSummary(.,
                "lppd")
  
  # Posterior predictive samples
  basereg_postpred <- tryCatch({
    posterior_predict(object = fit_basereg,
                      newdata = newdata,
                      cores = 1)},
    warning = function(w) {
      message(w)
    },
    error = function(e) {
      message(e)
    })
  
  # mean
  mean_sum <- basereg_postpred %>%
    apply(., 1, mean) %>% 
    calcSummary(.,
                "mean")
  mean_data <- calcSummary(mean(y),
                           "mean_data")
  mean_sum_bp <- basereg_postpred %>%
    apply(., 1, function(x) mean(mean(x) > mean(y))) %>% 
    calcSummary(.,
                "mean_bp")
  
  # median
  median_sum <- basereg_postpred %>%
    apply(., 1, median) %>%
    calcSummary(.,
                "median")
  median_data <- calcSummary(median(y),
                             "median_data")
  median_sum_bp <- basereg_postpred %>%
    apply(., 1, function(x) mean(median(x) > median(y))) %>% 
    calcSummary(.,
                "median_bp")
  
  # variance
  var_sum <- basereg_postpred %>%
    apply(., 1, var) %>% 
    calcSummary(.,
                "var")
  var_data <- calcSummary(var(y),
                          "var_data")
  var_sum_bp <- basereg_postpred %>%
    apply(., 1, function(x) mean(var(x) > var(y))) %>% 
    calcSummary(.,
                "var_bp")
  
  # min
  min_sum <- basereg_postpred %>%
    apply(., 1, min) %>% 
    calcSummary(.,
                "min")
  min_data <- calcSummary(min(y),
                          "min_data")
  min_sum_bp <- basereg_postpred %>%
    apply(., 1, function(x) mean(min(x) > min(y))) %>% 
    calcSummary(.,
                "min_bp")
  
  # max
  max_sum <- basereg_postpred %>%
    apply(., 1, max) %>% 
    calcSummary(.,
                "max")
  max_data <- calcSummary(max(y),
                          "max_data")
  max_sum_bp <- basereg_postpred %>%
    apply(., 1, function(x) mean(max(x) > max(y))) %>% 
    calcSummary(.,
                "max_bp")
  
  # total
  total_sum <- basereg_postpred %>%
    apply(., 1, sum) %>% 
    calcSummary(.,
                "total")
  total_data <- calcSummary(sum(y),
                            "total_data")
  total_sum_bp <- basereg_postpred %>%
    apply(., 1, function(x) mean(sum(x) > sum(y))) %>% 
    calcSummary(.,
                "total_bp")
  
  # root mean squared error (RMSE)
  rmse <- basereg_postpred %>%
    apply(., 1, calcRMSE, y)
  rmse_sum <- rmse %>% 
    calcSummary(.,
                "rmse")
  
  # kolmogorov-smirnov test statistic
  ks_test_sum <- basereg_postpred %>%
    apply(., 1, calcKSTest, y) %>% 
    calcSummary(.,
                "ks_test")
  gc()
  
  result <- list(
    bayes_p = rbind(
      mean_sum_bp,
      median_sum_bp,
       var_sum_bp,
       min_sum_bp,
       max_sum_bp,
      total_sum_bp
    ) %>% round(., 3),
    desc = rbind(
      lppd_basereg_sum,
      rmse_sum,
      ks_test_sum,
      mean_sum,
      mean_data,
      median_sum,
      median_data,
      var_sum,
      var_data,
      min_sum,
      min_data,
      max_sum,
      max_data,
      total_sum,
      total_data
    ) %>% round(., 3),
    log_mean_lik = lppd_basereg,
    rmse = rmse)
  
  return(result)
  
}

calcBinRegStats <- function(fit_binreg, newdata, limit = NULL, optim_stat = "test_error", class_limits = NULL) {
  
  z <- newdata$data_kylla
  
  # LPPD
  if (nrow(newdata) < 30000) {
    lppd_binreg <- log_lik(fit_binreg, newdata = newdata, cores = 3) %>% 
      apply(., 2, rowOperLPPD)
  } else {
    loglik_binreg <- numeric(nrow(newdata))
    intervals <- seq(1, nrow(newdata), length.out = 10) %>% round()
    for (i in seq(1, (length(intervals)-1), 1)) {
      newdata_i <- newdata[intervals[i]:intervals[i+1],]
      loglik_binreg_i <- log_lik(fit_binreg, newdata = newdata_i, cores = 3)
      loglik_binreg[intervals[i]:intervals[i+1]] <- loglik_binreg_i %>%
        apply(., 2, rowOperLPPD)
      rm(loglik_binreg_i)
    }
  }
  
  lppd_binreg_sum <- lppd_binreg %>%
    sum() %>%
    calcSummary(.,
                "lppd")
  
  # Generate predictions using a classification limit
  postpred_epred <- posterior_epred(fit_binreg, newdata = newdata)
  
  if (is.null(limit)) {
    limit <- findLimit(epred = postpred_epred,
                       z = z,
                       limits = class_limits,
                       optim_stat = optim_stat)
  } 
  postpred_binreg <- apply(postpred_epred, 2, predictClass, limit)
  # postpred_binreg <- posterior_predict(fit_binreg,
  #                                      newdata = newdata,
  #                                      cores = 4)
  
  # proportion correct
  prop_cor <- postpred_binreg %>%
    apply(., 1, calcPropIncorrect, z)
  prop_cor_sum <- calcSummary(prop_cor,
                              "prop_incorrect")
  # proportion of false negatives
  prop_falseneg <- postpred_binreg %>%
    apply(., 1, calcFalseNeg, z)
  prop_falseneg_sum <- calcSummary(prop_falseneg,
                                   "prop_falseneg")
  
  # proportion of false positives
  prop_falsepos <- postpred_binreg %>%
    apply(., 1, calcFalsePos, z)
  prop_falsepos_sum <- calcSummary(prop_falsepos,
                                   "prop_falsepos")
  
  # F-score
  fscore <- postpred_binreg %>% 
    apply(., 1, FScore, z)
  fscore_sum <- calcSummary(fscore,
                            "fscore")
  result <- list(
    limit = limit,
    desc = rbind(
      lppd_binreg_sum,
      prop_cor_sum,
      prop_falseneg_sum,
      prop_falsepos_sum,
      fscore = fscore_sum),
    log_mean_lik = lppd_binreg
  )
  
  return(result)
  
}

calcLogLik <- function(loglik_binreg, loglik_basereg) {
  
  loglik_tp <- calcTwoPartLogLik(
    y = y,
    loglik_binreg = loglik_binreg,
    loglik_basereg = loglik_basereg
  )
  
  lppd_tp <- loglik_tp %>%
    apply(., 2, rowOperLPPD)
  
  lppd_tp_sum  <- lppd_tp %>%
    sum() %>% 
    calcSummary(., "lppd")
  
  return(list("lppd_tp" = lppd_tp,
              "lppd_sum" = lppd_tp_sum))
} 

saveLogLik <- function(fit_binreg, fit_basereg, newdata, respath) {
  
  loglik_binreg <- log_lik(fit_binreg,
                           newdata = newdata)
  loglik_basereg <- log_lik(fit_basereg,
                            newdata = newdata %>% filter(toimtuki_data > 0))
  loglik_tp <- calcLogLik(loglik_binreg = loglik_binreg,
                          loglik_basereg = loglik_basereg)
  
  saveRDS(loglik_tp, respath)
  return(loglik_tp)
}

calcTwoPartRegTests <- function(fit_binreg,
                                fit_basereg,
                                loglik_binreg,
                                loglik_basereg,
                                newdata,
                                limit = NULL
) {
  y <- newdata$toimtuki_data
  y_pos <- y[y > 0]
  z <- newdata$data_kylla
  
  # posterior predictive samples
  binreg_epred <- posterior_epred(fit_binreg,
                                  newdata = newdata)
  if (!is.null(limit)) {
    binreg_postpred <- apply(binreg_epred, 2, predictClass, limit)
  } else {
    message("Error: Limit is null.")
    return()
  }
  basereg_postpred <- posterior_predict(fit_basereg,
                                        newdata = newdata)
  tp_postpred <- binreg_postpred*basereg_postpred
  
  # proportion correct
  prop_cor <- binreg_postpred %>%
    apply(., 1, calcPropIncorrect, z)
  prop_cor_sum <- calcSummary(prop_cor,
                              "prop_incorrect")
  
  # proportion of false negatives
  prop_falseneg <- binreg_postpred %>%
    apply(., 1, calcFalseNeg, z)
  prop_falseneg_sum <- calcSummary(prop_falseneg,
                                   "prop_falseneg")
  
  # proportion of false positives
  prop_falsepos <-  binreg_postpred %>%
    apply(., 1, calcFalsePos, z)
  prop_falsepos_sum <- calcSummary(prop_falsepos,
                                   "prop_falsepos")
  
  # RMSE
  rmse <- tp_postpred %>%
    apply(., 1, calcRMSE, y)
  rmse_sum <- rmse %>% 
    calcSummary(.,
                "rmse")
  
  # kolmogorov-smirnov test
  ks_test_sum <- tp_postpred %>%
    apply(., 1, calcKSTest, y) %>% 
    calcSummary(.,
                "ks_test")
  
  # bayes p-values
  
  # mean
  mean_sum <- tp_postpred %>%
    apply(., 1, function(x) mean(x)) %>% 
    calcSummary(.,
                "mean")
  mean_data <- calcSummary(mean(y_pos),
                           "mean_data")
  mean_sum_bp <- tp_postpred %>%
    apply(., 1, function(x) {mean(mean(x) > mean(y))}) %>% 
    calcSummary(.,
                "mean_bp")
  
  # variance
  var_sum <- tp_postpred %>%
    apply(., 1, function(x) var(x)) %>% 
    calcSummary(.,
                "var")
  var_data <- calcSummary(var(y_pos),
                          "var_data")
  var_sum_bp <- tp_postpred %>%
    apply(., 1, function(x) mean(var(x) > var(y))) %>% 
    calcSummary(.,
                "var_bp")
  
  # max
  max_sum <- tp_postpred %>%
    apply(., 1, function(x) max(x)) %>% 
    calcSummary(.,
                "max")
  max_data <- calcSummary(max(y_pos),
                          "max_data")
  max_sum_bp <- tp_postpred %>%
    apply(., 1, function(x) mean(max(x) > max(y))) %>% 
    calcSummary(.,
                "max_bp")
  
  # total
  total_sum <- tp_postpred %>%
    apply(., 1, sum) %>% 
    calcSummary(.,
                "total")
  total_data <- calcSummary(sum(y),
                            "total_data")
  total_sum_bp <- tp_postpred %>%
    apply(., 1, function(x) mean(sum(x) > sum(y))) %>% 
    calcSummary(.,
                "total_bp")
  
  result <- tibble(
    file = fit_basereg$file,
    formula = as.character(fit_basereg$formula)[1],
    formula_binreg = as.character(fit_binreg$formula)[1],
    criteria = list(rbind(lppd_tp_sum,
                          rmse_sum,
                          ks_test_sum)),
    bayes_p = list(rbind(mean_sum_bp,
                         var_sum_bp,
                         max_sum_bp,
                         total_sum_bp)),
    pred_desc = list(rbind(mean_sum,
                           var_sum,
                           max_sum,
                           total_sum)),
    data_desc = list(rbind(mean_data,
                           median_data,
                           var_data,
                           min_data,
                           max_data,
                           total_data)),
    class_desc = list(rbind(prop_cor_sum,
                            prop_falseneg_sum,
                            prop_falsepos_sum)),
    log_mean_lik = list(lppd_tp)
  )
  
  return(result)
}

runBinRegTests <- function(fit_binreg,
                           train_data,
                           test_data,
                           result_path,
                           optim_stat,
                           class_limits) {
  
  if (is.null(class_limits)) {
    class_limits <- seq(0.2, 0.7, 0.1)
    warning("Warning: Limits to test were unspecified, defaults min = 0.2,
            max = 0.7, with steps of 0.1 used.")
  }
  
  train_tests <- calcBinRegStats(fit_binreg = fit_binreg,
                                 newdata = train_data,
                                 optim_stat = optim_stat,
                                 class_limits = class_limits)
  
  valid_tests <- calcBinRegStats(fit_binreg = fit_binreg,
                                 newdata = test_data,
                                 limit = train_tests$limit)
  
  # Compile results
  results <- tibble(
    formula = as.character(fit_binreg$formula)[1],
    file = fit_binreg$file,
    class_limit = train_tests$limit,
    train_criteria = list(train_tests$desc),
    train_log_lik = list(train_tests$log_mean_lik),
    train_plot = list(suppressMessages(pp_check(fit_binreg))),
    valid_criteria = list(valid_tests$desc),
    valid_log_lik = list(valid_tests$log_mean_lik),
    valid_plot = list(suppressMessages(pp_check(fit_binreg,
                                                newdata = test_data)))
  )
  
  saveRDS(results,
          result_path)
}

runBaseRegTests <- function(fit_basereg,
                            train_data,
                            test_data, 
                            result_path) {
  
  train_tests <- calcBaseRegStats(fit_basereg = fit_basereg,
                                  newdata = train_data)
  
  valid_tests <- calcBaseRegStats(fit_basereg = fit_basereg,
                                  newdata = test_data)
  
  # Compile results
  results <- tibble(
    formula = as.character(fit_basereg$formula)[[1]],
    file = fit_basereg$file,
    train_bayes_p = list(train_tests$bayes_p),
    train_desc = list(train_tests$desc),
    train_log_lik = list(train_tests$log_mean_lik),
    train_rmse = list(train_tests$rmse),
    train_plot = list(suppressMessages(
      pp_check(fit_basereg) + xlim(0, 20000)
    )),
    valid_bayes_p = list(valid_tests$bayes_p),
    valid_desc = list(valid_tests$desc),
    valid_log_lik = list(valid_tests$log_mean_lik),
    valid_rmse = list(valid_tests$rmse),
    valid_plot = list(suppressMessages(
      pp_check(fit_basereg,
               newdata = test_data) + xlim(0, 20000)
    ))
  )
  
  saveRDS(results,
          result_path)
  
}

calcAccuracy <- function(z, zrep, optim_stat) {
  
  res_class <- data.frame(
    test_error = apply(zrep, 1, calcPropIncorrect, z),
    false_neg = apply(zrep, 1, calcFalseNeg, z),
    false_pos = apply(zrep, 1, calcFalsePos, z))
  
  res_class <- pivot_longer(res_class,
                            cols = everything(),
                            names_to = "stat",
                            values_to = "estimate") %>% 
    mutate(estimate = estimate*100)
  res_class$optim_stat <- optim_stat
  
  return(res_class)
}


