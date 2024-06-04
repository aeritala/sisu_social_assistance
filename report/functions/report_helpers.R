
calcPlotDim <- function(pagedim, s) {
  if (pagedim == "width") {
    res <- 2480*s
  }
  if (pagedim == "height") {
    res <- 3508*s
  }
  return(res)
}

readResults <- function(files, all_files = TRUE) {
  if (isTRUE(all_files)) {
    files <- list.files(path = files,
                            full.names = TRUE) %>%
      stringr::str_subset(., "_res.Rds")
  }
  results <- Reduce(bind_rows, lapply(files, readRDS))
  return(results)
}

transpDf <- function(newdf) {
  df <- newdf %>% t() %>% data.frame()
  df$stat <- rownames(df)
  return(df)
}

tidyResults <- function(results, tests) {
  new_list <- lapply(results[[tests]], transpDf)
  new_summary <- Reduce(bind_rows, new_list)
  new_summary$file <- rep(results$file, each = 3)
  rownames(new_summary) <- NULL
  return(new_summary)
}

getVarNames <- function(real, train) {
  if (isTRUE(real) & isTRUE(train)) {
    vars <- c("train_desc", "train_bayes_p")
    names(vars) <- c("desc", "bayes_p")
  } else if (isTRUE(real) & isFALSE(train)) {
    vars <- c("valid_desc", "valid_bayes_p")
    names(vars) <- c("desc", "bayes_p")
  } else if (isFALSE(real) & isTRUE(train)) {
    vars <- "train_criteria"
    names(vars) <- c("desc")
  } else if (isFALSE(real) & isFALSE(train)) {
    vars <- "valid_criteria"
    names(vars) <- c("desc")
  }
  return(vars)
}

calcLogLikDiffs <- function(results = NULL, real = TRUE, train = FALSE, files = NULL, all_files = TRUE, tp = FALSE) {
  
  if (isFALSE(tp)) {
    var_names <- getVarNames(real = real,
                             train = train)
    desc <- var_names["desc"]
    
    if (isTRUE(train)) {
      loglik <- "train_log_lik"
    } else {
      loglik <- "valid_log_lik"
    }
  } else {
    loglik <- "log_mean_lik"
    desc <- "criteria"
  }
  
  # Selection criteria and descriptive stats
  if (is.null(results)) {
    results <- readResults(files = files,
                              all_files = all_files)
  }
  desc_summary <- tidyResults(results = results,
                              tests = desc)
  desc_means <- desc_summary %>%
    filter(stat == "mean")
  
  # Preallocate result df
  lppd_summary <- data.frame(
    "file" = results$file,
    "lppd_diff" = numeric(nrow(results)),
    "se_diff" = numeric(nrow(results))
    )
  
  # Max LPPD
  loglik_max_row <- which.max(desc_means$lppd)
  loglik_max_filename <- desc_means$file[loglik_max_row]
  
  # lppd_diff
  lppd_summary$lppd_diff <- -(desc_means$lppd[loglik_max_row] - desc_means$lppd)
  
  # Differences
  loglik_matrix <- suppressMessages(Reduce(bind_cols, results[[loglik]]))
  colnames(loglik_matrix) <- results$file
  diff_matrix <- apply(loglik_matrix, 2, \(x) loglik_matrix[[loglik_max_filename]] - x)
  
  # se_diff
  lppd_summary$se_diff <- apply(diff_matrix, 2, sd)*sqrt(nrow(diff_matrix)) 
  
  return(lppd_summary)
}

calcLogLikDiffsTP <- function(df) {
  
    # LPPD
    lppd_sums <- colSums(df)
    
    # LPPD_diff
    best <- names(df)[which.max(lppd_sums)]
    lppd_diff <- -(lppd_sums[[best]] - lppd_sums)
    
    # SE_diff
    lppd_diff_i <- -(df[[best]] - df)
    se_diff <- apply(lppd_diff_i, 2, sd)*sqrt(nrow(lppd_diff_i)) 
    
    # Collect results
    lppd_summary <- data.frame(
      "model" = names(df),
      "lppd" = lppd_sums,
      "lppd_diff" = lppd_diff,
      "se_diff" = se_diff,
      row.names = NULL)
    
    return(lppd_summary)
}

fitSummaryReal <- function(results, train = FALSE) {
  
  var_names <- getVarNames(real = TRUE,
                           train = train)
  desc <- var_names["desc"]
  bp_tests <- var_names["bayes_p"]
  
  # Selection criteria and descriptives
  desc_summary <- tidyResults(results, desc)
  
  # Descriptives
  desc_postmeans <- desc_summary %>%
    select(!c(lppd, rmse, ks_test, contains("data"))) %>%
    select(c(file, stat, !c(file, stat)))
  
  # Criteria
  desc_criteria <- desc_summary %>%
    select(c(file, stat, lppd, rmse, ks_test))
  
  # lppd compare
  lppd_compare <- calcLogLikDiffs(results,
                                  real = TRUE,
                                  train = train)
  
  desc_criteria <- left_join(desc_criteria, lppd_compare, by = "file") %>% 
    select(file, stat, lppd, lppd_diff, se_diff, rmse, ks_test)
  
  # Bayes p-values
  bp_summary <- tidyResults(results, bp_tests)
  bp_summary <- bp_summary %>% 
    select(c(file, stat, !c(file, stat)))
  
  # Summary of the data set
  data_summary <- desc_summary[1,] %>%
    select(., contains("data"))
  rownames(data_summary) <- NULL
  
  return(list("criteria" = desc_criteria,
              "bayesian_p" = bp_summary,
              "postpred_desc" = desc_postmeans,
              "data_desc" = data_summary))
}

fitSummaryBin <- function(results, train = FALSE) {
  
  var_names <- getVarNames(real = FALSE,
                           train = train)
  desc <- var_names["desc"]
  
  # Selection criteria and descriptives
  desc_summary <- tidyResults(results, desc)
  
  # Descriptives
  desc_postmeans <- desc_summary %>%
    select(!c(lppd, contains("data"))) %>%
    select(c(file, stat, !c(file, stat)))
  
  # Criteria
  desc_criteria <- desc_summary %>%
    select(c(file, stat, lppd))
  
  # lppd compare
  lppd_compare <- calcLogLikDiffs(results,
                                  real = FALSE,
                                  train = train)
  
  desc_criteria <- left_join(desc_criteria, lppd_compare, by = "file") %>% 
    select(file, stat, lppd, lppd_diff, se_diff)
  
  return(list("criteria" = desc_criteria,
              "postpred_desc" = desc_postmeans))
}