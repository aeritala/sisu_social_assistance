# setup ----
source(file = file.path("output",
                        "2024-01-11_gpareto",
                        "constants.R"))

vars <- expand.grid(reg.par = c("sigma", "k"),
                    prior = c("test01"),
                    f = f[2],
                    stringsAsFactors = FALSE)
h <- 3

# fits ----
writeSubModelTasks(task = task,
                   test_tag = paste0("h", h),
                   fit_tag = fit_tag,
                   dist = "generalized_pareto",
                   reg.par = vars$reg.par,
                   prior = vars$prior,
                   f = vars$f,
                   warmup = 500,
                   iter = 1500,
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
n <- 9:10
files <- file.path(task_path,
                   paste0("fit_", fit_tag,
                          ifelse(n < 10,
                                 paste0("0", n),
                                 n),
                          "_res.Rds"))
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
