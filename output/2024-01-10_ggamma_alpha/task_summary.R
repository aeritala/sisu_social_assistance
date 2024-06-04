# setup ----
source(file = file.path("output",
                        "2024-01-10_ggamma_alpha",
                        "constants.R"),
       local = TRUE)

# fit criteria and descriptions ----
n <- 1:13
files <- file.path(task_path,
                   paste0("fit_", fit_tag, ifelse(n < 10,
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
                          paste0("analysis_criteria.html")),
  knit_root_dir = getwd())
)

# brms summaries ----
suppressWarnings(rmarkdown::render(
  file.path("report",
            "functions",
            "analysis_brmssummaries.Rmd"),
  params = list(
    task = task
  ),
  output_format = "html_document",
  output_file = file.path("..", "..", task_path,
                          "analysis_brms_summaries.html")
  ))

# brms pp_check plots ----
suppressWarnings(rmarkdown::render(
  file.path("report",
            "functions",
            "analysis_plots.Rmd"),
  params = list(
    task = task
  ),
  output_format = "html_document",
  output_file = file.path(task_path,
                          paste0("analysis_pp_plots.html")),
  knit_root_dir = getwd()
))

fit12 <- readRDS(file.path(task_path, "fit_ggamma_alpha12.Rds"))
