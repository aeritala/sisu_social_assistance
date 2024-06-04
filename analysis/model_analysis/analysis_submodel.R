# Succinct analysis of a submodel

# Set up ----

suppressWarnings(suppressMessages(library(optparse)))

# Command line interface
options <- list(
  make_option("--file",
              type = "character",
              help = "Path to file containing a brms fit"),
  make_option("--mini",
              type = "logical",
              help = "TRUE for mini analysis (default) and FALSE for complete analysis"),
  make_option("--output",
              type = "character",
              help = "Path to the folder to which save the results are saved"),
  make_option("--optim_stat",
              type = "character",
              default = "test_error",
              help = "Which statistic is selected to optimise the classification limit. Currently supported
              options are fscore, test_error and cohenkappa."),
  make_option("--tag",
              type = "character",
              default = "",
              help = "Optional tag for the result file."),
  make_option("--minlim",
              type = "numeric",
              default = 0.2,
              help = "Minimum classification threshold"),
  make_option("--maxlim",
              type = "numeric",
              default = 0.1,
              help = "Maximum classification threshold"),
  make_option("--steplim",
              type = "numeric",
              default = 0.8,
              help = "Size of one step in calculation of the classification threshold"),
  make_option("--overwrite",
              type = "logical",
              default = TRUE,
              help = "If TRUE possible pre-existing analyis file will be overwritten,
              if FALSE, execution of the analysis script will be halted.")
  
)
parser <- OptionParser(usage = "Analysis of a brms fit",
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

if (opt$tag != "") {
  ext <- paste0("_", opt$tag, "_res.Rds")
} else {
  ext <- paste0("_res.Rds")
}
output_file <- paste0(basename(tools::file_path_sans_ext(opt$file)), ext)

if (file.exists(file.path(opt$output, output_file)) & (opt$overwrite == FALSE)) {
  message(paste("The analysis file", output_file, "already exists,
      execution of the analysis halted."))
  quit()
}

# Set up ----

suppressWarnings(suppressMessages(library(dplyr)))
suppressWarnings(suppressMessages(library(brms)))
suppressWarnings(suppressMessages(library(ggplot2)))

source(file = file.path(
  "analysis",
  "model_analysis",
  "analysis_helpers.R"
))

# Analysis ----

fit <- readRDS(opt$file)
fit$file <- basename(opt$file)

if (isTRUE(opt$mini)) {
  
  if (fit$family$family == "custom") {
    expose_functions(fit, vectorize = TRUE)
  }
  
  if (fit$family$family == "bernoulli") {
    x_max <- 1
  } else {
    x_max <- 20000 
  }
  
  print(summary(fit))

  X11()
  print(pp_check(fit) + xlim(0, x_max))
  while (!is.null(dev.list())) Sys.sleep(1)
  
} else {

  if (fit$family$family == "bernoulli") {
 
    limits <- seq(opt$minlim, opt$maxlim, opt$steplim)
    
    train_data <- readRDS(file.path(
      "no_vc",
      "cache",
      "sample_bintrain_standard.Rds"
    ))
    valid_data <- readRDS(file.path(
      "no_vc",
      "cache",
      "sample_binvalid_standard.Rds"
    ))
    
    runBinRegTests(fit_binreg = fit,
                   train_data = train_data,
                   test_data = valid_data,
                   result_path = file.path(
                     opt$output,
                     output_file),
                   optim_stat = opt$optim_stat,
                   class_limits = limits)
  } else {
    
    if (fit$family$family == "custom") {
      expose_functions(fit, vectorize = TRUE)
    }
    
    train_data <- readRDS(file.path(
      "no_vc",
      "cache",
      "sample_realtrain_standard.Rds"
    ))
    valid_data <- readRDS(file.path(
      "no_vc",
      "cache",
      "sample_realvalid_standard.Rds"
    ))

    runBaseRegTests(fit_basereg = fit,
                    train_data = train_data,
                    test_data = valid_data,
                    result_path = file.path(
                      opt$output,
                      output_file))
  }
  
}