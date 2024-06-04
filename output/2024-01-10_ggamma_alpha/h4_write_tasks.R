
# setup ----
source(file = file.path("output",
                        "2024-01-10_ggamma_alpha",
                        "constants.R"))
prev_best <- readRDS(file.path(task_path,
                               "fit_ggamma_alpha05_res.Rds"))
f <- prev_best$formula
h <- 4

# fits ----
writeSubModelTasks(task = task,
                   test_tag = paste0("h", h),
                   fit_tag = fit_tag,
                   dist = dist,
                   reg.par = "alpha",
                   prior = "informative",
                   f = f,
                   update.f = hypotheses[[h]],
                   warmup = 1000,
                   iter = 2000,
                   chains = 3,
                   seed = SEED,
                   overwrite = FALSE,
                   print = TRUE)

# analysis ----
writeAnalysisTasks(task = task,
                   fit_tag = fit_tag,
                   mini = FALSE,
                   overwrite = FALSE,
                   print = TRUE)

# report ----
n <- 5:7
files <- file.path(task_path,
                   paste0("fit_", fit_tag, "0", n, "_res.Rds"))
suppressWarnings(rmarkdown::render(
  file.path("report",
            "functions",
            "summary_submodel_real.Rmd"),
  params = list(
    task = task,
    files = files
  ),
  output_format = "html_document",
  output_file = file.path("..", "..", task_path,
                          paste0("h", h, "_analysis_results.html")),
  knit_root_dir = getwd()
  )
)

fit06res <- readRDS(files[2])
fit07res <- readRDS(files[3])
rownames(fit07res$train_desc[[1]])[nrow(fit07res$train_desc[[1]])] <- "total_data"

changeRName <- function(newres) {
  rownames(newres$train_desc[[1]])[1] <- "lppd"
  newres$train_desc[[1]] <- as.data.frame(newres$train_desc[[1]])
  newres$train_bayes_p[[1]] <- as.data.frame(newres$train_bayes_p[[1]])
  rownames(newres$valid_desc[[1]])[1] <- "lppd"
  newres$valid_desc[[1]] <- as.data.frame(newres$valid_desc[[1]])
  newres$valid_bayes_p[[1]] <- as.data.frame(newres$valid_bayes_p[[1]])
  return(newres)
}

fit06res <- changeRName(fit06res)
fit06res
fit07res <- changeRName(fit07res)
fit07res

saveRDS(fit06res, file.path(task_path, "fit_ggamma_alpha06_res.Rds"))
saveRDS(fit07res, file.path(task_path, "fit_ggamma_alpha07_res.Rds"))
