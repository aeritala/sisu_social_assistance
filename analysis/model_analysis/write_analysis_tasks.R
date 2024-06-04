# Set up ----
library(dplyr)

source(file = file.path("models",
                        "functions",
                        "write_helpers.R"),
       local = TRUE)

# Helper function to write submodel analysis tasks ----
writeAnalysisTasks <- function(task, fit_path = NA, test_tag = NA, file_tag = NA, fit_tag = NA, mini,
                               print = FALSE, overwrite = TRUE, tp = FALSE, binfile, realfile,
                               optim_stat = NA, minlim = NA, maxlim = NA, steplim = NA) {
  
  # tp <- FALSE
  # mini <- FALSE
  # overwrite <- FALSE
  # optim_stat <- NA
  # minlim <- NA
  # maxlim <- NA
  # steplim <- NA
  # file_tag <- NA
  
  output <- file.path("output",
                      task)
  # if (is.na(fit_path)) {
  #   fit_path <- output
  # }
  
  files <- list.files(task_path, full.names = TRUE) %>%
    stringr::str_subset(paste0("_", fit_tag, "\\d+\\.Rds"))
  
  if (isFALSE(tp)) {
    script <- file.path("analysis",
                        "model_analysis",
                        "analysis_submodel.R",
                        fsep = "\\")
    
    newoptions <- list(
      file = addParentheses(files),
      mini = mini,
      output = addParentheses(output),
      overwrite = overwrite
    )
    
  } else {
    script <- file.path("analysis",
                        "model_analysis",
                        "analysis_tpmodel.R",
                        fsep = "\\")
    
    newoptions <- list(
      binfile = addParentheses(file.path(output, binfile)),
      realfile = addParentheses(file.path(output, realfile)),
      output = addParentheses(output),
      overwrite = overwrite
    )
  }
  
  if (!is.na(optim_stat)) {
    newoptions$optim_stat <- addParentheses(optim_stat)
    newoptions$minlim <- minlim
    newoptions$maxlim <- maxlim
    newoptions$steplim <- steplim
  }
  
  if(!is.na(file_tag)) newoptions$tag = addParentheses(file_tag)
  
  newoptions <- newoptions[!is.na(newoptions)] %>% 
    as.data.frame()
  
  newtask <- apply(newoptions, 1, function(x) {
    writeOption(names(newoptions), x) %>%
      paste(., collapse = " ")
  })
  
  commands <- paste(
    "Rscript --no-save --no-restore",
    script,
    newtask,
    "\n",
    collapse = ""
  )
  
  # Save the task commands
  output_file <- paste0("analysis_tasks_", fit_tag, ".bat")
  if (!is.na(test_tag)) {
    output_file <- paste0(test_tag, "_", output_file)
  }
  output_file <- file.path(output,
                           output_file)
  cat(commands,
      file = output_file)
  
  if (isTRUE(print)) {
    cat(commands)
  }
  
}