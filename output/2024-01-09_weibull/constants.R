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
SEED <- 0109
task <- "2024-01-09_weibull"
task_path <- file.path("output",
                       task)
#dir.exists(task_path)
fit_tag <- "weibull"
dist <- "weibull"