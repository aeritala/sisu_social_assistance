
source(file = file.path("output",
                        "2024-03-12_tp",
                        "constants.R"),
       local = TRUE)

# bern_best <- readRDS(file.path(bern_path,
#                                "fit_bernoulli11_res.Rds"))
# gamma_best <- readRDS(file.path(gamma_path,
#                                 "fit_gamma10_res.Rds"))
# weib_best <- readRDS(file.path(weib_path,
#                                   "fit_weibull16_res.Rds"))
# ggamma_best <- readRDS(file.path(ggamma_path,
#                                 "fit_ggamma_alpha13_res.Rds"))
# 
# bern_f <- bern_best$formula
# gamma_f <- gamma_best$formula
# weib_f <- weib_best$formula
# ggamma_f <- ggamma_best$formula


writeAnalysisTasks(task = "2024-03-12_tp",
                   fit_path = "2024-01-31_tp",
                   fit_tag = "tp_fscore4",
                   file_tag = "fscore4",
                   mini = FALSE,
                   overwrite = TRUE,
                   print = TRUE,
                   tp = TRUE,
                   binfile = c("fit_tp01.Rds"),
                   realfile = c("fit_tp02.Rds", "fit_tp03.Rds", "fit_tp04.Rds"))


n <- 1:4
files <- file.path(task_path,
                   paste0("fit_tp", ifelse(n < 10,
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

