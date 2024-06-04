# load helpers ----
suppressMessages(suppressWarnings(library(dplyr)))
source(file = file.path("analysis",
                        "model_analysis",
                        "write_submodel_tasks.R"),
       local = TRUE)
source(file = file.path("analysis",
                        "model_analysis",
                        "write_analysis_tasks.R"),
       local = TRUE)
source(file = file.path("models",
                        "tasks",
                        "hypotheses.R"),
       local = TRUE)

# set up ----
SEED <- 0110
task <- "2024-01-10_ggamma_mu"
task_path <- file.path("output",
                       task)
#dir.exists(task_path)
fit_tag <- "ggamma_mu"
dist <- "generalized_gamma"