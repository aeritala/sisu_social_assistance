# Set up ----
library(dplyr)

source(file = file.path("models",
                        "functions",
                        "write_helpers.R"),
       local = TRUE)

writeSubModelTasks <- function(task, test_tag = NULL, fit_tag, dist, seed, f, update.f = NA,
         prior = NA, reg.par = NA, warmup = NA, iter = NA,
         chains = NA, overwrite = NA, print = FALSE, tp = FALSE) {
  
  output <- file.path("output",
                      task)
  script <- file.path("models",
                      "functions",
                      "fit_submodel.R",
                      fsep = "\\")
  
  if (isTRUE(tp)) {
    newdata <- ifelse(dist == "bernoulli",
                      "sample_train_standard.Rds",
                      "sample_trainpos_standard.Rds")

  } else {
    
    if (any("bernoulli" %in% dist)) {
      newdata <- ifelse(dist == "bernoulli",
                        "sample_bintrain_standard.Rds",
                        "sample_realtrain_standard.Rds")
    } else {
      newdata <- "sample_realtrain_standard.Rds"
    }

  }
  
  
  
  newoptions <- list(
    data = addParentheses(file.path("no_vc",
                                    "cache",
                                    newdata,
                                    fsep = "\\")),
    dist = addParentheses(dist),
    prior = addParentheses(prior),
    reg.par = ifelse(!is.na(reg.par), addParentheses(reg.par), reg.par),
    f = addParentheses(f),
    update.f = ifelse(!is.na(update.f), addParentheses(update.f), update.f),
    warmup = warmup,
    iter = iter,
    chains = chains,
    file = "",
    overwrite = overwrite,
    seed = seed)
  
  newoptions <- newoptions[!is.na(newoptions)] %>% 
    as.data.frame()
  rows <- 1:nrow(newoptions)
  
  fit_indices <- list.files(output) %>%
      stringr::str_subset(paste0("_", fit_tag, "\\d+\\.Rds")) %>% 
      stringr::str_extract("\\d\\d")
  fit_indices <- fit_indices[!is.na(fit_indices)] %>% as.numeric()
  max_index <- suppressWarnings(max(fit_indices))
  if (is.finite(max_index)) {rows <- rows + max_index }
  
  newoptions$file <- c(addParentheses(
    paste0(file.path("output",
                     task,
                     paste0("fit_",
                            fit_tag),
                     fsep = "\\"),
           ifelse(rows < 10,
                  paste0("0", rows, ".Rds"),
                  paste0(rows, ".Rds"))
    )))
  print(newoptions)
  
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
  output_file <- paste0("submodel_tasks_", fit_tag, ".bat")
  if (!is.null(test_tag)) {
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
