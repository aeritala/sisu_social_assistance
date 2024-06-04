
# setup ----
source(file = file.path("output",
                        "2024-01-10_ggamma_alpha",
                        "constants.R"))
prev_best <- readRDS(file.path(task_path,
                               "fit_ggamma_alpha02_res.Rds"))
f <- prev_best$formula
h <- 2

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
n <- 2:4
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
  knit_root_dir = getwd())
)