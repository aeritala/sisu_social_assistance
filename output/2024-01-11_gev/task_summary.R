# setup ----
source(file = file.path("output",
                        "2024-01-11_gev",
                        "constants.R"),
       local = TRUE)


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

# priors ----
suppressWarnings(rmarkdown::render(
  file.path("report",
            "functions",
            "analysis_priors.Rmd"),
  params = list(
    task = task
  ),
  output_format = "html_document",
  output_file = file.path(task_path,
                          "analysis_priors.html")
  ))


# trace plots ----
fit1 <- readRDS(file.path(task_path, "fit_gev01.Rds"))

