# Succinct analysis of a submodel

# Set up ----

suppressWarnings(suppressMessages(library(optparse)))

# Command line interface
options <- list(
  make_option("--binfile",
              type = "character",
              help = "Path to file containing a brms fit for the binary part of the model"),
  make_option("--realfile",
              type = "character",
              help = "Path to file containing a brms fit for the continuous part of the model"),
  make_option("--output",
              type = "character",
              default = NULL,
              help = "Path to the folder to which save the results are saved"),
  make_option("--tag",
              type = "character",
              default = "",
              help = "An optional tag for the result files"),
  make_option("--overwrite",
              type = "logical",
              default = TRUE,
              help = "If TRUE possible pre-existing analyis file will be overwritten,
              if FALSE, execution of the analysis script will be halted.")
  
)
parser <- OptionParser(usage = "Analysis of two part model",
                       option_list = options)

opt <- parse_args(parser)
required_opts <- vapply(options, function(x) x@dest, character(1))
actual_opts <- setdiff(names(opt), "help")
for (req in required_opts) {
  if (!(req %in% actual_opts)) {
    msg <- paste0("Argument '", req, "' missing with no default.", "\n")
    stop(msg)
  }
}

# Check if the file already exists ----

if (!(opt$tag == "")) {
  res_tag <- paste0("_valid_", opt$tag, "_res.Rds")
} else {
  res_tag <- "_valid_res.Rds"
}
output_file_valid <- paste0(
  basename(tools::file_path_sans_ext(opt$realfile)), res_tag)

if (file.exists(file.path(opt$output, output_file_valid)) & (isFALSE(opt$overwrite))) {
  message(paste("The analysis file", output_file_valid, "already exists,
      execution of the enalysis halted."))
  quit()
}

# Set up ----

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(brms)))
suppressWarnings(suppressMessages(library(ggplot2)))
suppressWarnings(suppressMessages(library(tidyr)))

source(file = file.path(
  "analysis",
  "model_analysis",
  "analysis_helpers.R"
))

# Test
# opt <- list()
# opt$output <- file.path("output", "2024-05-16_tp")
# opt$binfile <- file.path(opt$output, "fit_tp01.Rds")
# opt$realfile <- file.path(opt$output, "fit_tp02.Rds")

# Load models and data ----
fit_binreg <- readRDS(opt$binfile)
fit_basereg <- readRDS(opt$realfile)
fit_binreg$file <- basename(opt$binfile)
fit_basereg$file <- basename(opt$realfile)
l_ck <- readRDS(file.path(opt$output, "limit_ck.Rds"))
l_te <- readRDS(file.path(opt$output, "limit_te.Rds"))
limits <- c("cohens_kappa" = l_ck, "test_error" = l_te, "none" = -1)

test_data <- readRDS(file.path(
  "no_vc",
  "cache",
  "sample_test_standard.Rds"
))
y <- test_data$toimtuki_data

if (fit_basereg$family$family == "custom") {
  expose_functions(fit_basereg, vectorize = TRUE)
}

# Analysis ----

if (fit_basereg$family$family == "custom") {
  newdist <- "ggamma"
} else {
  newdist <- fit_basereg$family$family
}

# Log-likelihood for the test data set

loglik_path <- file.path(opt$output, paste0("loglik_", newdist, ".Rds"))

if (!file.exists(loglik_path)) {
  message("Saving log-likelihood for the test data...")
  loglik_tp <- saveLogLik(fit_binreg, fit_basereg, test_data, loglik_path)
} else {
  loglik_tp <- readRDS(loglik_path)
}

# Posterior predictive samples

message("Calculating RMSE and KS-test for the test data...")
# y <- test_data$toimtuki_data
draws <- 3000
class_stat <- names(limits)
g <- length(class_stat)
criteria_res <- list(dist = newdist,
                     class_stat = rep(class_stat, each = 2*g),
                     criteria = rep(rep(c("rmse", "ks_test"), each = g), g),
                     stat = rep(c("estimate", "lwr_ci", "upr_ci"), times = 2*g),
                     est = numeric(3*g))
bayesp_res <- list(dist = newdist,
                   class_stat = rep(class_stat, each = draws),
                   mean = numeric(draws*g),
                   total = numeric(draws*g),
                   var = numeric(draws*g),
                   q99 = numeric(draws*g))

# Criteria calculated for the test data set
basereg_ppred <- posterior_predict(fit_basereg,
                                   draw_ids = 1:draws,
                                   newdata = test_data)
k <- 1
h <- 1
for (i in seq_along(limits)) {
  message(paste(i, Sys.time()))

  if (limits[i] != -1) {
    # Predictions with classification limit
    binreg_epred <- posterior_epred(fit_binreg,
                                    draw_ids = 1:draws,
                                    newdata = test_data)
    if (!is.null(limits)) {
      binreg_pred <- apply(binreg_epred, 2, predictClass, limits[i])
    } else {
      message("Error: Limit is null.")
      return()
    }
    tp_pred <- binreg_pred*basereg_ppred
  } else {
    # Predictions without classification limit
    binreg_ppred <- posterior_predict(fit_binreg,
                                      draw_ids = 1:draws,
                                      newdata = test_data)
    tp_pred <- binreg_ppred*basereg_ppred
  }
  
  # RMSE
  rmse <- tp_pred %>%
    apply(., 1, calcRMSE, y)
  rmse_sum <- calcSummaryNV(rmse)
  
  criteria_res$est[k:(k+2)] <- rmse_sum
  k <- k+3
  
  # Kolmogorov-smirnov test
  ks_test_sum <- tp_pred %>%
    apply(., 1, calcKSTest, y) %>% 
    calcSummaryNV(.)
  criteria_res$est[k:(k+2)] <- ks_test_sum
  k <- k+3
  
  # Bayes p-values
  # Mean
  mean_pp <- apply(tp_pred, 1, mean)
  # Total
  total_pp <- mean_pp*ncol(tp_pred)
  # Variance
  var_pp <- apply(tp_pred, 1, var)
  # 90% quantile
  q99_pp <- apply(tp_pred, 1, quantile, 0.99)
  
  bayesp_res$mean[h:(h+draws-1)] <- mean_pp
  bayesp_res$total[h:(h+draws-1)] <- total_pp
  bayesp_res$var[h:(h+draws-1)] <- var_pp
  bayesp_res$q99[h:(h+draws-1)] <- q99_pp
  h <- h + draws
}

criteria_res_df <- data.frame(criteria_res)
bayesp_res_df <- data.frame(bayesp_res) %>% 
  pivot_longer(., cols = !c(dist, class_stat),
               names_to = "stat",
               values_to = "est")

message("Saving the results...")
saveRDS(list("loglik" = loglik_tp,
             "criteria" = criteria_res_df,
             "bayesp" = bayesp_res_df),
        file.path(opt$output,
                  output_file_valid))

